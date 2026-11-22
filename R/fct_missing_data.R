#' Describe the Included Items' Missing Responses and Information
#' @param data Item data, before any complete-case filtering.
#' @param items Included item names.
#' @param fit Optional fitted continuous model.
#' @return Counts, patterns, coverage, and parameter-specific FMI when available.
#' @author Ken Kelley
#' @noRd
sieve_missing_diagnostics <- function(data, items = names(data), fit = NULL) {
  X <- sieve_reliability_data(data, items)
  counts <- sieve_reliability_counts(X)
  out <- list(counts = counts, patterns = NULL, coverage = NULL,
    fmi = NULL, reason = NULL, warnings = character())
  for (kind in c("patterns", "coverage")) {
    read <- sieve_capture(switch(kind,
      patterns = DMAR::missing_patterns(X),
      coverage = DMAR::missing_coverage(X)
    ))
    out[[kind]] <- read$value
    out$warnings <- c(out$warnings, read$warnings, read$error)
  }
  if (!is.null(fit) && counts$complete < counts$rows) {
    read <- sieve_capture(DMAR::fraction_missing_information(fit))
    out$fmi <- read$value
    out$reason <- read$error
    out$warnings <- c(out$warnings, read$warnings)
  }
  out
}

#' A Shared Respondent and Missing Information Line
#' @param diagnostics A result of sieve_missing_diagnostics().
#' @param used Respondents used by the fitted model.
#' @param treatment Actual treatment of missing responses.
#' @return Plain text with full counts and display-only rounded quantities.
#' @author Ken Kelley
#' @noRd
sieve_missing_line <- function(diagnostics, used, treatment) {
  counts <- diagnostics$counts
  text <- sprintf("Respondents used: %d; answered every included item: %d of %d; treatment: %s.",
    used, counts$complete, counts$rows, treatment)
  table <- diagnostics$fmi
  if (is.data.frame(table) && any(is.finite(table$fmi))) {
    at <- which.max(replace(table$fmi, !is.finite(table$fmi), -Inf))
    text <- paste(text, sprintf("Largest fraction of missing information: %.3f; normal-theory interval width ratio: %.3f; effective N: %.1f (parameter %s).", table$fmi[at], table$width_inflation[at], table$N_effective[at], table$term[at]))
    text <- paste(text, "FMI and these precision ratios describe normal-theory working-model information, not the inflation of robust or cluster intervals.")
    if (counts$observed < 500L) text <- paste(text, "FMI can itself be imprecise below several hundred respondents (Chen & Savalei, 2021).")
  } else if (counts$complete < counts$rows) {
    text <- paste(text, "Parameter-specific missing information is unavailable for this fit.")
  }
  text
}

#' Resolve the Optional Robust Two-Stage Sensitivity Fit
#' @param spec A continuous ML-family specification.
#' @param enabled Whether the researcher requested two-stage estimation.
#' @return A specification with its exact missing-data, SE, and test choices.
#' @author Ken Kelley
#' @noRd
sieve_two_stage_spec <- function(spec, enabled = FALSE) {
  if (!isTRUE(enabled)) return(spec)
  if (length(spec$ordered) || !spec$estimator %in% c("ML", "MLR")) {
    spec$problems <- c(spec$problems, "Robust two-stage ML requires continuous indicators. Choose a continuous-item estimator or retain WLSMV.")
    return(spec)
  }
  if (!is.null(spec$engine_args$cluster)) {
    spec$problems <- c(spec$problems, "Cluster-robust two-stage inference is not supplied. Use the clustered FIML model.")
    return(spec)
  }
  spec$estimator <- "ML"
  spec$missing <- "robust.two.stage"
  sieve_spec_engine_args(spec, list(se = "robust.two.stage", test = "satorra.bentler"))
}

#' The Residual Test Beside an Optional Two-Stage Fit
#'
#' The residual based statistic of Savalei and Bentler (2009, Eq. 8; Eq. 14
#' with auxiliary variables), computed by `DMAR::sem_fit_measures()` from a
#' supplied lavaan fit by normal theory two-stage ML (`missing =
#' "two.stage"`), whose `chi_square` row it is. DMAR reports lavaan's scaled
#' statistic for the robust two-stage fit on screen, so the model is refit
#' by normal theory two-stage ML for this statistic; both stages are the
#' same, so on the same respondents the refit has the robust fit's
#' estimates (only the standard errors and the test differ). The statistic
#' weights the residuals of the stage one moments by the observed
#' information of the saturated model, so it is referred to chi square
#' under normal data missing at random; DMAR does not report its
#' distribution free form, which performs poorly in small samples (Savalei
#' & Bentler, 2009). Under nonnormality the robust test is the scaled
#' statistic of the robust two-stage fit. As semTools' `twostage()` did,
#' the refit treats every observed variable as random (`fixed.x = FALSE`),
#' as the statistic is derived.
#'
#' The refit describes the fit on screen (`fit`). That fit may hold
#' observed covariates fixed (lavaan's default `fixed.x = TRUE`), and then
#' lavaan sets aside every respondent missing one of them, where a refit
#' with the covariates random would keep those respondents and so estimate
#' from other data. The refit therefore uses the respondents the fit on
#' screen used (`lavaan::lavInspect(fit, "case.idx")`), and the result says
#' so. Its estimates are then compared with the fit's, parameter by
#' parameter: the display says they are the same only when every shared
#' parameter agrees within `tolerance` (relative to the larger of 1 and the
#' estimate), and otherwise says by how much they differ. A refit that used
#' a different number of respondents gives no statistic. A DMAR refusal of
#' class `dmar_refusal` (for example `dmar_two_stage_rows_unavailable`) is
#' recognized by its class, and every reason DMAR gives is returned in
#' `error`.
#'
#' @param spec Resolved two-stage specification.
#' @param data Aligned raw model data (the data the fit on screen was given).
#' @param fit The robust two-stage fit on screen, or `NULL` (then every
#'   respondent with an observed value is used and the estimates are not
#'   compared).
#' @param tolerance The largest relative difference between the estimates of
#'   the fit and the refit for them to be called the same.
#' @return A list with `value` (a one-row data frame: `statistic`, `chisq`,
#'   `df`, `pvalue`, at full precision), `error` (the reason no statistic
#'   was computed, or `NULL`), `error_class`, `refused` (`TRUE` for a DMAR
#'   refusal recognized by its class), `warnings`, `rows` (`NULL`, or how
#'   the refit's respondents were chosen; see `sieve_two_stage_rows()`),
#'   `agreement` (`NULL`, or the comparison of the estimates; see
#'   `sieve_two_stage_agreement()`), and `code` (the R that reproduces it,
#'   reading the model `cfa_model` or `sem_model`, its data, and its fit as
#'   the recorded fit defines them).
#' @author Ken Kelley
#' @noRd
sieve_two_stage_residual <- function(spec, data, fit = NULL, tolerance = 1e-3) {
  args <- sieve_spec_args(spec)
  args$estimator <- "ML"
  args$missing <- "two.stage"
  args$se <- NULL
  args$test <- NULL
  args$fixed.x <- FALSE
  args$conditional.x <- FALSE
  sem <- identical(spec$fun, "sem")
  rows <- sieve_two_stage_rows(fit, data)
  if (!is.null(rows)) data <- data[rows$used, , drop = FALSE]
  args$data <- data
  out <- list(value = NULL, error = NULL, error_class = NULL, refused = FALSE,
    warnings = character(0), rows = rows, agreement = NULL,
    code = sieve_two_stage_residual_code(spec, rows = rows))
  refit <- sieve_capture(do.call(if (sem) lavaan::sem else lavaan::cfa, args))
  out$warnings <- refit$warnings
  if (!refit$ok) {
    out$error <- paste("The normal theory two-stage refit for the residual statistic could not be fit:", refit$error)
    out$error_class <- refit$error_class
    return(out)
  }
  if (!is.null(fit)) {
    out$agreement <- sieve_two_stage_agreement(fit, refit$value, tolerance = tolerance)
    out$code <- sieve_two_stage_residual_code(spec, rows = rows, same = out$agreement$same)
    if (!identical(out$agreement$n_fit, out$agreement$n_refit)) {
      out$error <- sprintf(paste(
        "The normal theory two-stage refit used %d respondents where the robust two-stage fit",
        "used %d, so its residual statistic would describe other data; none is shown."
      ), out$agreement$n_refit, out$agreement$n_fit)
      return(out)
    }
  }
  read <- sieve_capture(DMAR::sem_fit_measures(refit$value))
  out$warnings <- unique(c(out$warnings, read$warnings))
  if (!read$ok) {
    out$error_class <- read$error_class
    out$refused <- "dmar_refusal" %in% read$error_class
    out$error <- paste(
      if (out$refused) "DMAR refused the residual statistic for this fit:" else "DMAR gave no residual statistic for this fit:",
      read$error
    )
    return(out)
  }
  v <- stats::setNames(read$value$value, read$value$term)
  out$value <- data.frame(
    statistic = "Savalei-Bentler residual based chi square (normal theory)",
    chisq = unname(v[["chi_square"]]), df = unname(v[["df"]]),
    pvalue = unname(v[["p_chi_square"]]), stringsAsFactors = FALSE
  )
  out$test <- attr(read$value, "sem_fit")$test
  out
}

#' The Respondents a Two-Stage Fit Used, When a Refit Would Use Others
#'
#' A two-stage fit that holds observed covariates fixed (`fixed.x = TRUE`
#' or `conditional.x = TRUE`) uses only the respondents with every one of
#' them observed; a refit with the covariates random would keep the others.
#' Respondents with no observed value at all are left out by both, so they
#' do not count as set aside.
#'
#' @param fit The fit on screen, or `NULL`.
#' @param data The data the fit was given.
#' @return `NULL` when the fit used every respondent with an observed
#'   value (or there is no fit); otherwise a list with `used` (the rows of
#'   `data` the fit used, in order), `set_aside` (how many respondents with
#'   an observed value it did not use), `rows` (the rows of `data`), and
#'   `covariates` (the observed covariates held fixed that are missing for
#'   some respondent set aside).
#' @author Ken Kelley
#' @noRd
sieve_two_stage_rows <- function(fit, data) {
  if (is.null(fit)) return(NULL)
  used <- sort(as.integer(unlist(lavaan::lavInspect(fit, "case.idx"))))
  observed <- which(rowSums(!is.na(data)) > 0L)
  set_aside <- setdiff(observed, used)
  if (!length(set_aside)) return(NULL)
  options <- lavaan::lavInspect(fit, "options")
  covariates <- if (isTRUE(options$fixed.x) || isTRUE(options$conditional.x)) {
    x <- intersect(lavaan::lavNames(fit, "ov.x"), names(data))
    x[vapply(x, function(nm) anyNA(data[set_aside, nm]), logical(1))]
  } else character(0)
  list(used = used, set_aside = length(set_aside), rows = nrow(data), covariates = covariates)
}

#' Compare the Estimates of a Two-Stage Fit and Its Normal Theory Refit
#'
#' @param fit The robust two-stage fit on screen.
#' @param refit The normal theory two-stage refit.
#' @param tolerance The largest difference, relative to the larger of 1 and
#'   the fit's estimate, for the estimates to be called the same.
#' @return A list with `parameters` (the number of free parameters the two
#'   share), `max_abs` and `max_rel` (the largest absolute and relative
#'   differences, at full precision), `parameter` (where the relative one
#'   is largest), `same`, `tolerance`, and `n_fit` and `n_refit` (the
#'   respondents each used).
#' @author Ken Kelley
#' @noRd
sieve_two_stage_agreement <- function(fit, refit, tolerance = 1e-3) {
  a <- lavaan::coef(fit)
  b <- lavaan::coef(refit)
  shared <- intersect(names(a), names(b))
  gap <- abs(unname(a[shared]) - unname(b[shared]))
  rel <- gap / pmax(1, abs(unname(a[shared])))
  at <- if (length(rel)) which.max(rel) else integer(0)
  list(
    parameters = length(shared),
    max_abs = if (length(gap)) max(gap) else NA_real_,
    max_rel = if (length(rel)) max(rel) else NA_real_,
    parameter = if (length(at)) shared[at] else NA_character_,
    same = length(rel) > 0L && all(is.finite(rel)) && max(rel) <= tolerance,
    tolerance = tolerance,
    n_fit = as.integer(sum(lavaan::lavInspect(fit, "nobs"))),
    n_refit = as.integer(sum(lavaan::lavInspect(refit, "nobs")))
  )
}

#' The Residual Test Beside a Two-Stage Fit, for the Page
#' @param test A result of `sieve_two_stage_residual()`.
#' @return Tags: the statistic (rounded here, for display only) and what it
#'   is, or the reason there is none.
#' @author Ken Kelley
#' @noRd
sieve_two_stage_residual_ui <- function(test) {
  if (is.null(test$value)) {
    return(shiny::tagList(
      shiny::tags$b("Savalei-Bentler Residual Test"),
      shiny::p(class = "small", test$error %||% "No residual statistic was computed for this fit.")
    ))
  }
  v <- test$value
  shown <- data.frame(
    Statistic = v$statistic,
    `Chi square` = sprintf("%.3f", v$chisq),
    df = format(v$df, trim = TRUE),
    p = if (is.finite(v$pvalue) && v$pvalue < 1e-4) "< .0001" else sprintf("%.4f", v$pvalue),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  shiny::tagList(
    shiny::tags$b("Savalei-Bentler Residual Test"),
    sieve_plain_table_ui(shown),
    shiny::p(class = "small text-muted", sieve_two_stage_residual_sentence(test))
  )
}

#' The Sentence That Says How the Residual Statistic Was Computed
#'
#' Says which respondents the refit used and claims that its estimates are
#' those of the fit on screen only when they were compared and agree.
#'
#' @param test A result of `sieve_two_stage_residual()` with a value.
#' @return One string (numbers rounded here, for display only).
#' @author Ken Kelley
#' @noRd
sieve_two_stage_residual_sentence <- function(test) {
  rows <- test$rows
  agreement <- test$agreement
  plural <- function(n, one, many) if (n == 1L) one else many
  who <- if (is.null(rows)) {
    ""
  } else if (length(rows$covariates)) {
    sprintf(paste0(
      " on the %d respondents the robust two-stage fit used: that fit holds the observed ",
      "covariate%s %s fixed, so lavaan set aside the %d respondent%s missing %s, and the ",
      "refit sets aside the same ones"
    ),
    length(rows$used), plural(length(rows$covariates), "", "s"),
    paste(rows$covariates, collapse = ", "), rows$set_aside,
    plural(rows$set_aside, "", "s"),
    plural(length(rows$covariates), "it", "one of them"))
  } else {
    sprintf(" on the %d respondents the robust two-stage fit used", length(rows$used))
  }
  same <- if (is.null(agreement)) {
    ""
  } else if (isTRUE(agreement$same)) {
    sprintf(
      " Its estimates are those of the robust two-stage fit (largest difference %s over %d shared parameters).",
      format(signif(agreement$max_abs, 2)), agreement$parameters
    )
  } else {
    sprintf(paste0(
      " Its estimates differ from those of the robust two-stage fit by up to %s (%s), so the ",
      "statistic describes the refit, not the fit on screen."
    ), format(signif(agreement$max_abs, 3)), agreement$parameter)
  }
  paste0(
    "The residual based statistic of Savalei and Bentler (2009, Eq. 8; Eq. 14 with ",
    "auxiliary variables), computed by DMAR's sem_fit_measures() from the same model ",
    "refit by normal theory two-stage ML, with every observed variable treated as random",
    who, ".", same, " It weights the residuals of the ",
    "first-stage moments by the observed information of the saturated model, so it is ",
    "referred to chi square under normal data missing at random. Under nonnormality the ",
    "test to read is the scaled statistic of the robust two-stage fit in the fit table."
  )
}

#' The R That Reproduces the Residual Statistic Beside a Two-Stage Fit
#' @param spec Resolved two-stage specification.
#' @param rows `NULL`, or the respondents the refit used (see
#'   `sieve_two_stage_rows()`): the script then takes the rows the
#'   recorded fit used.
#' @param same Whether the refit's estimates were found to be those of the
#'   fit (`NULL` when they were not compared).
#' @return Character vector of R code lines, reading the model, data, and
#'   fit objects the recorded two-stage fit defines.
#' @author Ken Kelley
#' @noRd
sieve_two_stage_residual_code <- function(spec, rows = NULL, same = NULL) {
  sem <- identical(spec$fun, "sem")
  model <- if (sem) "sem_model" else "cfa_model"
  data <- if (sem) "sem_data" else "cfa_model_data"
  fit <- if (sem) "sem_fit" else "cfa_fit"
  extra <- spec$engine_args %||% list()
  extra <- extra[setdiff(names(extra), c("se", "test", "fixed.x", "conditional.x", "bootstrap"))]
  extra_code <- vapply(names(extra), function(nm) paste0(nm, " = ", paste(utils::capture.output(dput(extra[[nm]])), collapse = "")), character(1))
  subset <- if (!is.null(rows)) {
    c(
      if (length(rows$covariates)) {
        sprintf(
          "# The robust two-stage fit holds %s fixed, so lavaan used only the respondents with %s observed; the refit uses the same rows.",
          paste(rows$covariates, collapse = ", "), if (length(rows$covariates) == 1L) "it" else "them"
        )
      } else {
        "# The refit uses the rows the robust two-stage fit used."
      },
      sprintf("%s_ts <- %s[sort(unlist(lavaan::lavInspect(%s, \"case.idx\"))), , drop = FALSE]", data, data, fit)
    )
  }
  c(
    "# The Savalei-Bentler (2009) residual based statistic beside the robust two-stage fit:",
    if (isTRUE(same)) {
      "# the same model refit by normal theory two-stage ML (its estimates are the robust fit's), every observed"
    } else {
      "# the same model refit by normal theory two-stage ML, every observed"
    },
    "# variable random, and the statistic read by DMAR (the chi_square, df and p_chi_square rows).",
    subset,
    sprintf("%s_ts <- lavaan::%s(%s, data = %s,", fit, if (sem) "sem" else "cfa", model,
      if (is.null(rows)) data else paste0(data, "_ts")),
    paste0("  ", paste(c(
      "estimator = \"ML\"", sprintf("std.lv = %s", if (isTRUE(spec$std_lv)) "TRUE" else "FALSE"),
      if (isTRUE(spec$orthogonal)) "orthogonal = TRUE",
      "missing = \"two.stage\"", "fixed.x = FALSE", "conditional.x = FALSE", extra_code,
      if (length(extra$aux)) "check.post = FALSE"
    ), collapse = ", "), ")"),
    sprintf("two_stage_residual <- DMAR::sem_fit_measures(%s_ts)", fit),
    "two_stage_residual[two_stage_residual$term %in% c(\"chi_square\", \"df\", \"p_chi_square\"), ]"
  )
}

#' Missing Information Beside the Parameters of a Fitted Model
#' @param ns Module namespace.
#' @return A card with parameter-specific diagnostics and optional intervals.
#' @author Ken Kelley
#' @noRd
sieve_missing_info_ui <- function(ns) {
  bslib::card(bslib::card_header("Information Lost to Missing Responses"),
    shiny::uiOutput(ns("missing_info_note")),
    DT::DTOutput(ns("missing_info_table")),
    sieve_settings("FMI Interval Settings", ids = ns(c("missing_B", "missing_level", "missing_seed")),
      sieve_conf_level_input(ns("missing_level")),
      sieve_boot_inputs(ns, B = 1000L, B_id = "missing_B", seed_id = "missing_seed", min_B = 400L)),
    shiny::actionButton(ns("missing_ci"), "Compute FMI Intervals", class = "btn-primary"),
    shiny::p(class = "small text-muted",
      "FMI is parameter specific. Its largest value is a precision diagnostic, not a rule for changing the estimator. The effective N is N(1 - FMI), and interval width inflation is 1/sqrt(1 - FMI). These are normal-theory working-model diagnostics, not the inflation of robust or cluster intervals. They can themselves be imprecise below several hundred respondents (Chen & Savalei, 2021). Bootstrap intervals run only on request; their limits appear beside the original estimates.")
  )
}

#' Wire the Fitted Model's Missing Information Table
#' @param input,output,session Module interfaces.
#' @param state Shared study state.
#' @param parent Reactive model result.
#' @param current Reactive currentness predicate.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_missing_info_server <- function(input, output, session, state, parent, current) {
  request <- shiny::reactive(list(
    B = sieve_finite_or(input$missing_B, 1000L),
    level = sieve_finite_or(input$missing_level, .95),
    seed = sieve_seed_input(input$missing_seed)))
  intervals <- shiny::eventReactive(input$missing_ci, {
    shiny::req(isTRUE(current()))
    run <- parent()
    shiny::req(run$fit)
    settings <- request()
    B <- settings$B
    level <- settings$level
    read <- sieve_capture({
      request <- sieve_dmar_boot_request(B, conf_level = level)
      if (!request$ok) stop(request$message, call. = FALSE)
      rng <- sieve_draw_seed(settings$seed)
      options <- lavaan::lavInspect(run$fit, "options")
      call <- tryCatch(run$fit@call, error = function(e) NULL)
      clustered <- !is.null(call$cluster) || grepl("cluster", options$se %||% "", fixed = TRUE)
      if (clustered || !is.null(sieve_sampling_problem(state))) {
        stop("A whole-cluster FMI bootstrap is not supplied. Point FMI remains normal-theory working-model information; no independent-row bootstrap was run.", call. = FALSE)
      }
      # One call that reports nothing until it returns: say so while it runs.
      shiny::withProgress(
        message = sprintf("Bootstrapping FMI intervals (DMAR): B = %d", as.integer(B)),
        detail = "One call to DMAR::fraction_missing_information(), which refits the model on each resample and reports its result when all of them finish.",
        value = 1,
        DMAR::fraction_missing_information(run$fit,
          ci_method = "percentile", conf_level = level, B = B, seed = rng$seed))
    })
    if (read$ok) sieve_log(state, "Parameter-specific missing information with percentile intervals", c(
      if (identical(run$spec$fun, "sem")) sieve_sem_code(run$spec, fit_name = "missing_fit") else sieve_cfa_code(run$spec, fit_name = "missing_fit"),
      sprintf("missing_information <- DMAR::fraction_missing_information(missing_fit, ci_method = \"percentile\", conf_level = %s, B = %d, seed = %d)",
        sieve_r_literal(level), B, rng$seed)))
    list(fit = run$fit, request = settings, read = read)
  }, ignoreNULL = TRUE)
  information <- shiny::reactive({
    if (!isTRUE(current())) return(list(reason = "Fit or update the model for the current included items."))
    run <- parent()
    if (is.null(run$fit)) return(list(reason = "Fit a model before inspecting missing information."))
    diagnostics <- attr(run$fit, "sieve_missing_data", exact = TRUE)
    held <- if (isTRUE((input$missing_ci %||% 0L) > 0L)) intervals() else NULL
    if (!is.null(held) && identical(held$fit, run$fit)) {
      if (!identical(held$request, request())) return(list(table = diagnostics$fmi,
        reason = "FMI interval settings changed. Press Compute FMI Intervals for the current confidence level, replication count and seed; no bootstrap was rerun."))
      return(list(table = held$read$value %||% diagnostics$fmi, reason = held$read$error))
    }
    list(table = diagnostics$fmi, reason = diagnostics$reason %||%
      if (is.null(diagnostics$fmi)) "No incomplete analyzed item responses, or parameter-specific FMI is unavailable for this estimator." else
        "FMI confidence intervals have not been requested. Press Compute FMI Intervals to estimate them.")
  })
  output$missing_info_note <- shiny::renderUI({
    settings <- request()
    checked <- sieve_capture(sieve_dmar_boot_request(settings$B, conf_level = settings$level))
    rule <- if (checked$ok) {
      if (checked$value$ok) sprintf("DMAR requires at least %d requested replications at this confidence level; additional replications may be needed when refits fail.", checked$value$required) else checked$value$message
    } else checked$error
    shiny::p(class = "small text-muted", information()$reason, " ", rule)
  })
  output$missing_info_table <- DT::renderDT({
    tab <- information()$table
    shiny::req(tab)
    sieve_dt(as.data.frame(tab), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10)) |>
      DT::formatRound(intersect(names(tab), c("fmi", "fmi_ci_lower", "fmi_ci_upper", "width_inflation", "N_effective")), 3)
  })
  invisible(NULL)
}

#' Describe Missing Responses for the Reliability Measurement Models
#' @param results Construct reliability records already fitted for the table.
#' @return Model-specific missing information and original item-frame counts.
#' @author Ken Kelley
#' @noRd
sieve_reliability_missing_line <- function(results) {
  valid <- Filter(function(r) identical(r$H$status$code, "ok") &&
    !is.null(r$H$missing_diagnostics), results)
  if (!length(valid)) return("")
  largest <- vapply(valid, function(r) {
    tab <- r$H$missing_diagnostics$fmi
    if (is.data.frame(tab) && any(is.finite(tab$fmi))) max(tab$fmi[is.finite(tab$fmi)]) else -Inf
  }, numeric(1))
  at <- if (any(is.finite(largest))) which.max(largest) else 1L
  r <- valid[[at]]
  counts <- lapply(valid, function(r) list(counts = r$H$missing_diagnostics$counts,
    used = r$H$n$used, treatment = r$H$missing$used))
  varies <- length(unique(vapply(counts, function(x) paste(
    x$used, x$counts$complete, x$counts$rows, x$treatment, sep = "/"), character(1)))) > 1L
  paste0("Missing-response diagnostic for the one-factor measurement model of ",
    r$spec$construct, if (is.finite(largest[at])) " (largest parameter FMI among the admissible models shown)" else " (example model)",
    ": ", sieve_missing_line(r$H$missing_diagnostics, r$H$n$used, r$H$missing$used),
    if (is.finite(largest[at])) " This is model-parameter FMI, not FMI or interval inflation for omega." else "",
    if (varies) " Respondent counts or treatment vary across constructs; each construct's counts are reported below." else "")
}

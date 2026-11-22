#' Compute Optional AVE Intervals From the Current Fitted Model
#'
#' Delegates the fitted-model percentile bootstrap to DMAR. The point
#' model, factor identities, confidence level, replication counts and
#' refusal status are retained. No group or level is averaged here.
#'
#' @param fit The current fitted measurement model.
#' @param B Requested replications, passed to DMAR as given (a count below
#'   DMAR's minimum is refused, not raised).
#' @param seed Recorded seed of this run.
#' @param conf_level Confidence level of the two-sided intervals.
#' @param sampling_unit Declared sampling unit; only independent rows are
#'   supported by this DMAR bootstrap.
#' @return A list with `table`, `counts`, `status`, `warnings`, and the
#'   engine record. A refused request carries no interval table.
#' @author Ken Kelley
#' @noRd
sieve_ave_intervals <- function(fit, B = 1000L, seed = NULL, conf_level = .95,
                                 sampling_unit = "independent") {
  request <- sieve_dmar_boot_request(B, conf_level = conf_level)
  no <- function(why) list(table = NULL, counts = NULL,
    status = list(code = "unavailable", message = why), warnings = character(0))
  if (!request$ok) return(no(request$message))
  if (!identical(sampling_unit, "independent")) {
    return(no("AVE intervals resample independent rows. No interval was run for the declared clustered or repeated sampling design."))
  }
  if (is.null(fit) || !sieve_has_solution(fit)) return(no("Fit the current measurement model before requesting AVE intervals."))
  run <- sieve_dmar_boot_run(list(fun = "average_variance_extracted", args = list(
    fit = fit, ci_method = "percentile", B = B, seed = seed, conf_level = conf_level)))
  if (!run$ok) {
    out <- no(run$error)
    out$status$code <- if (identical(sieve_dmar_boot_outcome(run), "refused")) "unavailable" else "failed"
    out$warnings <- run$warnings
    out$engine <- run
    return(out)
  }
  tab <- run$value
  sieve_dmar_require_columns(tab, c("factor", "ave", "ci_lower", "ci_upper"), "AVE intervals")
  # When every factor is improper, DMAR returns its point table and
  # diagnostics without resampling or attaching bootstrap counts. That is
  # a refusal for this fitted solution, not a changed result schema.
  count_fields <- c("B", "B_used", "n_dropped", "B_required", "bootstrap_status")
  no_bootstrap <- all(vapply(count_fields, function(field)
    is.null(attr(tab, field, exact = TRUE)), logical(1L)))
  if (!any(is.finite(tab$ave)) && no_bootstrap) {
    out <- no("DMAR returned no finite factor AVE and did not run its percentile bootstrap. Read the engine diagnostics for the fitted solution.")
    out$warnings <- run$warnings
    out$engine <- run
    return(out)
  }
  counts <- sieve_dmar_boot_result(tab, "AVE intervals")
  complete <- all(is.finite(tab$ci_lower) & is.finite(tab$ci_upper))
  list(table = tab, counts = counts,
    status = list(code = if (complete) "ok" else "partial", message = sprintf(
      "%d of %d resamples used; engine status: %s. %s",
      counts$used, counts$attempted, counts$status,
      if (complete) "Intervals are available for every factor." else "Some factor intervals were withheld; read the engine diagnostics.")),
    warnings = run$warnings, engine = run, conf_level = conf_level,
    fit_signature = sieve_lavaan_signature(fit), engine_identity = sieve_engine_identity())
}

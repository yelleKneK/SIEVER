#' Minimum requested replications for a DMAR result
#'
#' DMAR's request rule counts ten replications beyond each percentile
#' limit, fifty beyond each nominal BCa limit, and a floor of 100.
#' This is a request rule, not a guarantee about usable replications or
#' interval coverage. BCa can require more at its adjusted positions.
#' SIEVER's own cluster bootstrap uses its separately stated rule.
#'
#' @param method One of `"percentile"`, `"bca"`, `"se"`, or `"p"`.
#' @param conf_level Confidence level strictly between zero and one.
#' @param sidedness `"two"` for an interval or `"one"` for a bound.
#' @return The minimum requested count, or `NA_integer_` if it exceeds
#'   R's integer range.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_required <- function(method = c("percentile", "bca", "se", "p"),
                                     conf_level = 0.95, sidedness = c("two", "one")) {
  method <- match.arg(method)
  sidedness <- match.arg(sidedness)
  sieve_check_conf_level(conf_level)
  if (method %in% c("se", "p")) return(100L)
  tail <- if (identical(sidedness, "two")) (1 - conf_level) / 2 else
    min(conf_level, 1 - conf_level)
  required <- max(100, ceiling((if (identical(method, "bca")) 50 else 10) / tail - 1e-8))
  if (!is.finite(required) || required > .Machine$integer.max) return(NA_integer_)
  as.integer(required)
}

#' Validate a DMAR bootstrap request without changing its count
#' @param B Requested number of replications.
#' @param method,conf_level,sidedness See `sieve_dmar_boot_required()`.
#' @return A list with `ok`, the unchanged `B`, `required`, and `message`.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_request <- function(B, method = "percentile", conf_level = 0.95,
                                    sidedness = "two") {
  required <- sieve_dmar_boot_required(method, conf_level, sidedness)
  whole <- is.numeric(B) && length(B) == 1L && is.finite(B) &&
    B >= 1 && B <= .Machine$integer.max && B == floor(B)
  valid <- whole && !is.na(required) && B >= required
  message <- if (valid) NULL else if (is.na(required)) {
    "This confidence level requires more replications than R can represent as an integer. Choose a lower confidence level."
  } else if (!whole) {
    sprintf("The number of replications must be a positive whole number; this DMAR request requires at least %d. No replications were run.", required)
  } else {
    sprintf("DMAR requires at least %d requested replications for this %s%% %s %s (%s were entered). Enter a larger count; no replications were run.",
      required, format(100 * conf_level),
      if (identical(sidedness, "one")) "one-sided" else "two-sided",
      if (identical(method, "bca")) "BCa interval" else if (identical(method, "percentile")) "percentile result" else
        if (identical(method, "se")) "bootstrap standard error" else "bootstrap p-value",
      format(B, scientific = FALSE))
  }
  list(ok = valid, B = B, required = required, message = message)
}

#' Read DMAR's structured bootstrap result counts
#' @param result A DMAR result with replication attributes.
#' @param context The analysis name for a schema diagnostic.
#' @return The attempted, used, dropped and required counts and status.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_result <- function(result, context = "DMAR bootstrap") {
  fields <- c("B", "B_used", "n_dropped", "B_required", "bootstrap_status")
  values <- lapply(fields, function(nm) attr(result, nm, exact = TRUE))
  names(values) <- fields
  counts <- values[c("B", "B_used", "n_dropped", "B_required")]
  good <- vapply(counts, function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 0 && x == floor(x), logical(1L))
  status <- values$bootstrap_status
  if (!all(good) || !is.character(status) || length(status) != 1L || is.na(status) ||
      values$B_used + values$n_dropped != values$B) {
    stop(sprintf("%s returned an unsupported bootstrap count schema; no counts or interval were inferred.", context), call. = FALSE)
  }
  list(attempted = as.integer(values$B), used = as.integer(values$B_used),
    dropped = as.integer(values$n_dropped), required = as.integer(values$B_required),
    status = status)
}

#' Defined Response-Pattern Review Screens
#'
#' Longstring is the longest run of exactly equal raw responses, in the supplied
#' item order. Missing responses break the run by default; skipping them is an
#' explicit alternative. Flags are descriptive review rules, never exclusion
#' decisions. They are meaningful only for comparable response scales and a
#' defensible presentation order. No reverse scoring is performed here.
#' @param data Raw responses in the declared item order.
#' @param min_run Minimum run length; NULL uses half the selected pool rounded up.
#' @param min_answered Minimum answered items for longstring and invariant flags.
#' @param missing Whether missing responses break or skip the run.
#' @param irv_tol Within-person SD at or below this value is flagged.
#' @param md_alpha Chi-square tail cutoff for the descriptive Mahalanobis screen.
#' @return Counts, a case table, the resolved rule, and unavailable reasons.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' # Rounded scores stand in for item responses on a common scale.
#' screens <- sieve_response_screens(round(items), min_run = 4L)
#' screens[c("n_resp", "n_long", "n_irv", "n_md")]
#' head(screens$tbl)
#' @export
sieve_response_screens <- function(data, min_run = NULL, min_answered = 3L,
                                   missing = c("break", "skip"), irv_tol = 0,
                                   md_alpha = .001) {
  missing <- match.arg(missing)
  if (!is.data.frame(data) || ncol(data) < 1L) stop("Select response items first.", call. = FALSE)
  read <- sieve_numeric_columns(data, names(data))
  bad <- read$problems$kind %in% c("not_numeric", "not_finite")
  if (any(bad)) stop(paste(read$text[bad], collapse = " "), call. = FALSE)
  X <- as.matrix(read$data)
  integer_rule <- function(x, name, minimum) {
    if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x != trunc(x) || x < minimum) {
      stop(sprintf("%s must be one integer at least %d.", name, minimum), call. = FALSE)
    }
    as.integer(x)
  }
  n_item <- ncol(X)
  cut <- integer_rule(min_run %||% max(2L, ceiling(n_item / 2)), "Run cutoff", 2L)
  minimum <- integer_rule(min_answered, "Minimum answered", 2L)
  if (length(irv_tol) != 1L || !is.finite(irv_tol) || irv_tol < 0) stop("The SD cutoff must be nonnegative.",
    call. = FALSE
  )
  if (length(md_alpha) != 1L || !is.finite(md_alpha) || md_alpha <= 0 || md_alpha >= 1) stop("The tail cutoff must be between zero and one.",
    call. = FALSE
  )
  longest <- function(r) {
    if (!any(is.finite(r))) return(NA_integer_)
    best <- run <- 0L
    previous <- NA_real_
    for (v in r) {
      if (is.na(v)) {
        if (identical(missing, "break")) {
          run <- 0L
          previous <- NA_real_
        }
        next
      }
      run <- if (run > 0L && isTRUE(v == previous)) run + 1L else 1L
      previous <- v
      best <- max(best, run)
    }
    best
  }
  n <- nrow(X)
  answered <- rowSums(!is.na(X))
  long <- if (n) apply(X, 1L, longest) else integer()
  irv <- if (n) apply(
    X,
    1L,
    function(r) if (sum(!is.na(r)) >= 2L) stats::sd(r,
      na.rm = TRUE
    ) else NA_real_
  ) else numeric()
  eligible <- answered >= minimum
  md <- rep(NA_real_, n)
  cc <- stats::complete.cases(X)
  md_reason <- NULL
  if (sum(cc) > n_item + 1L) {
    read_md <- tryCatch(
      list(value = stats::mahalanobis(
        X[cc, , drop = FALSE],
        colMeans(X[cc, , drop = FALSE]), stats::cov(X[cc, , drop = FALSE])
      ), error = NULL),
      error = function(e) list(value = rep(NA_real_, sum(cc)), error = conditionMessage(e))
    )
    md[cc] <- read_md$value
    md_reason <- read_md$error
  } else md_reason <- "Too few complete cases for the covariance-based distance."
  p_md <- stats::pchisq(md, df = n_item, lower.tail = FALSE)
  flag_long <- eligible & is.finite(long) & long >= cut
  flag_irv <- eligible & is.finite(irv) & irv <= irv_tol
  flag_md <- is.finite(p_md) & p_md < md_alpha
  flagged_by <- vapply(seq_len(n), function(i) paste(c("Longstring", "Within-person SD", "Mahalanobis")
  [c(flag_long[i], flag_irv[i], flag_md[i])], collapse = "; "), character(1))
  rule <- list(
    items = names(data), values = "raw before reverse scoring", order = "supplied item order",
    min_run = cut, min_answered = minimum, missing = missing,
    irv_tol = irv_tol, md_alpha = md_alpha
  )
  list(
    tbl = data.frame(
      row = seq_len(n), answered = answered, eligible = eligible,
      longstring = long, irv = irv, mahalanobis = md, p_mahalanobis = p_md,
      flags = as.integer(flag_long) + as.integer(flag_irv) + as.integer(flag_md),
      flagged_by = flagged_by, stringsAsFactors = FALSE
    ),
    n_item = n_item, long_cut = cut, min_answered = minimum, missing = missing,
    n_long = sum(flag_long), n_irv = sum(flag_irv), n_md = sum(flag_md),
    n_md_eval = sum(is.finite(p_md)), n_complete = sum(cc), n_resp = n,
    irv_tol = irv_tol, md_alpha = md_alpha, rule = rule, unavailable = md_reason
  )
}

#' All observations, with stable identity and reversible inclusion state
#' @param state Shared application state.
#' @param flags Optional current response-screen table.
#' @param response_items Raw response columns to show.
#' @return One row per original observation, including excluded observations.
#' @author Ken Kelley
#' @noRd
sieve_observation_review <- function(state, flags = NULL, response_items = character()) {
  shiny::isolate({
    raw <- state$raw
    if (is.null(raw)) return(data.frame())
    ids <- sieve_case_ids(state)
    held <- state$removed_cases %||% list()
    included <- data.frame(
      original_row = ids$row, identifier = ids$id, status = "included",
      reason = "", flagged_by = "", stringsAsFactors = FALSE
    )
    excluded <- if (length(held)) do.call(rbind, lapply(held, function(r) data.frame(
      original_row = as.integer(r$row), identifier = as.character(r$id %||% NA_character_),
      status = "excluded", reason = as.character(r$reason %||% ""),
      flagged_by = as.character(r$flagged_by %||% ""), stringsAsFactors = FALSE
    ))) else included[FALSE, ]
    out <- rbind(included, excluded)
    if (anyDuplicated(out$original_row)) stop("Observation identities are duplicated; reopen a valid project before editing inclusion.",
      call. = FALSE
    )
    all_raw <- raw
    for (r in held) all_raw <- sieve_cases_bind(all_raw, r$raw)
    if (!is.null(flags) && nrow(flags)) {
      at <- match(out$original_row, flags$row)
      for (name in intersect(
        c(
          "answered",
          "longstring",
          "irv",
          "flags"
        ),
        names(flags)
      )) out[[name]] <- flags[[name]][at]
      current <- out$status == "included" & !is.na(at)
      out$flagged_by[current] <- flags$flagged_by[at[current]]
    }
    for (name in intersect(
      response_items,
      names(all_raw)
    )) out[[paste0(
      "Response: ",
      name
    )]] <- sieve_case_id_text(all_raw[[name]])
    out <- out[order(out$original_row), , drop = FALSE]
    rownames(out) <- NULL
    attr(out, "identifier") <- attr(ids, "variable")
    out
  })
}

#' Change observation inclusion through the common reversible service
#' @param state Shared state.
#' @param rows Stable original row references.
#' @param status Included or excluded.
#' @param reason Researcher rationale.
#' @param screening Optional resolved screening rule and current flags.
#' @return Common removal/restore result.
#' @author Ken Kelley
#' @noRd
sieve_set_case_status <- function(state, rows, status = c("excluded", "included"),
                                  reason = "", screening = NULL) {
  status <- match.arg(status)
  shiny::isolate({
    parsed <- suppressWarnings(as.numeric(rows))
    if (any(!is.finite(parsed) | parsed != trunc(parsed))) stop("Observation rows must be stable integer original-row references.",
      call. = FALSE
    )
    rows <- unique(as.integer(parsed))
    before <- sieve_revision(state)
    ids <- sieve_case_ids(state)
    flags <- if (is.list(screening) && is.data.frame(screening$tbl)) {
      stats::setNames(
        ifelse(is.na(screening$tbl$flagged_by) | !nzchar(screening$tbl$flagged_by),
          "not flagged",
          screening$tbl$flagged_by
        ),
        screening$tbl$row
      )
    } else NULL
    out <- if (identical(
      status,
      "excluded"
    )) sieve_remove_cases(state,
      rows,
      reason,
      flagged_by = flags
    ) else sieve_restore_cases(
      state,
      rows
    )
    changed <- if (identical(status, "excluded")) out$removed else out$restored
    if (length(changed)) {
      rule <- screening$rule %||% NULL
      if (identical(status, "excluded") && !is.null(rule)) {
        held <- state$removed_cases
        for (i in seq_along(held)) if (held[[i]]$row %in% changed) held[[i]]$screening <- rule
        state$removed_cases <- held
      }
      history <- state$case_actions %||% list()
      history[[length(history) + 1L]] <- sieve_audit_stamp(list(
        status = status, rows = as.integer(changed),
        reason = as.character(reason), screening = rule, before = before, after = sieve_revision(state),
        time = format(Sys.time(), tz = "UTC", usetz = TRUE)
      ), state)
      state$case_actions <- history
    }
    out
  })
}

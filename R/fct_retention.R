# fct_retention.R
# The evidence on the number of factors, computed as plain functions so the
# same code serves the Dimensionality tab, the tests, and R use. Nothing
# here chooses a number of factors. Each criterion is reported on its own:
# what it suggests, the correlations it was actually computed from, whether
# it ran, and, when it did not, the reason the engine gave. A change from
# the requested correlations to another kind is an analysis change and is
# carried in the result, never applied silently. The two stochastic parts
# (the parallel analysis reference and the random starts of the oblimin
# rotation behind Very Simple Structure) run on a local stream from a
# recorded seed, so the recorded R replays them exactly. The parallel
# analysis is DMAR::parallel_analysis(): one call per kind of reference
# data from the recorded seed, on the page's correlation basis (formed by
# DMAR from the item responses where its raw-data route forms the same
# matrix, passed as S with N otherwise); the page runs a long one in a
# background R process.
#
# Estimation is DMAR's for the parallel analysis (with stats::factanal for
# the fixed one-factor reduction of the scree plot, DMAR's reduction) and
# for the solutions behind Very Simple Structure (DMAR::fa_exploratory; the
# criterion is Revelle and Rocklin's), psych's for the polychoric,
# tetrachoric and mixed matrices, and lavaan's for the model fit by k. All
# values are kept at full precision.

#' Labels of the correlation bases
#'
#' @return A named character vector: internal identifier to display label.
#' @author Ken Kelley
#' @noRd
sieve_retention_basis_labels <- function() {
  c(
    pearson = "Pearson", polychoric = "polychoric",
    tetrachoric = "tetrachoric", mixed = "mixed"
  )
}

#' Display label of a correlation basis
#'
#' @param basis A basis identifier, or `NA`.
#' @return A character vector of labels; `"none"` where no basis applies.
#' @author Ken Kelley
#' @noRd
sieve_retention_basis_label <- function(basis) {
  out <- unname(sieve_retention_basis_labels()[as.character(basis)])
  out[is.na(out)] <- "none"
  out
}

#' The correlation basis a request resolves to
#'
#' With `"auto"` the basis follows the confirmed measurement level of the
#' included items: Pearson when every item is continuous, tetrachoric when
#' every item is binary, polychoric when every item is ordered or binary,
#' and mixed (Pearson among continuous items, polychoric or tetrachoric
#' among ordered or binary items, polyserial or biserial between the two
#' kinds) when the pool holds both kinds. `"poly"` asks for polychoric
#' correlations for every item (tetrachoric when all are binary) whatever
#' the confirmed levels are.
#'
#' @param choice `"auto"`, `"pearson"`, or `"poly"`.
#' @param types Character vector of confirmed item types.
#' @return One of `"pearson"`, `"polychoric"`, `"tetrachoric"`, `"mixed"`.
#' @author Ken Kelley
#' @noRd
sieve_retention_request <- function(choice = c("auto", "pearson", "poly"), types) {
  choice <- match.arg(choice)
  types <- as.character(types)
  types[is.na(types)] <- "continuous"
  categorical <- types %in% c("ordered", "binary")
  all_binary <- length(types) > 0L && all(types == "binary")
  if (identical(choice, "pearson")) {
    return("pearson")
  }
  if (identical(choice, "poly")) {
    return(if (all_binary) "tetrachoric" else "polychoric")
  }
  if (!any(categorical)) {
    return("pearson")
  }
  if (all_binary) {
    return("tetrachoric")
  }
  if (all(categorical)) "polychoric" else "mixed"
}

#' The choices of a tab's Correlations control
#'
#' The study's setting first (value `"auto"`, which follows
#' `state$estimator_choice` through `sieve_study_basis_request()`), then
#' Pearson and polychoric (tetrachoric when every item is binary) for this
#' tab alone.
#'
#' @param study_label The name of the first choice, which says what the
#'   study's setting resolves to (see `sieve_study_basis_label()`).
#' @return A named character vector: labels as names, codes as values.
#' @author Ken Kelley
#' @noRd
sieve_basis_choices <- function(study_label = "The study's setting (Ingest tab)") {
  stats::setNames(
    c("auto", "pearson", "poly"),
    c(study_label, "Pearson", "Polychoric or tetrachoric")
  )
}

#' The correlations the study's setting resolves to
#'
#' @param state The shared state store (read reactively).
#' @param types The confirmed types of the included items.
#' @return A basis identifier (see `sieve_retention_basis_labels()`).
#' @author Ken Kelley
#' @noRd
sieve_study_basis <- function(state, types) {
  sieve_retention_request(sieve_study_basis_request(state), types)
}

#' The first choice of a Correlations control, with what it resolves to
#'
#' @inheritParams sieve_study_basis
#' @return A label such as "The study's setting (Ingest tab): polychoric
#'   correlations".
#' @author Ken Kelley
#' @noRd
sieve_study_basis_label <- function(state, types) {
  sprintf(
    "The study's setting (Ingest tab): %s correlations",
    sieve_retention_basis_label(sieve_study_basis(state, types))
  )
}

#' Where the correlations in force on a tab come from, in words
#'
#' @param choice The value of the tab's Correlations control (`"auto"`,
#'   `"pearson"`, or `"poly"`).
#' @inheritParams sieve_study_basis
#' @return One sentence naming the correlations requested and where the
#'   request was made.
#' @author Ken Kelley
#' @noRd
sieve_basis_source_text <- function(choice, state, types) {
  choice <- if (length(choice) == 1L && choice %in% c("auto", "pearson", "poly")) choice else "auto"
  if (!identical(choice, "auto")) {
    return(sprintf(
      "In force here: %s correlations, chosen on this tab.",
      sieve_retention_basis_label(sieve_retention_request(choice, types))
    ))
  }
  study <- sieve_estimator_effective("study", state)
  why <- switch(study,
    mlr = "which treats the items as continuous (its Estimator control is set to MLR)",
    ml_fiml = "which treats the items as continuous (its Estimator control is set to ML with FIML)",
    ml = "which treats the items as continuous (its Estimator control is set to ML)",
    wlsmv = "which treats the items as ordered (its Estimator control is set to WLSMV)",
    "which follows the confirmed item types"
  )
  sprintf(
    "In force here: %s correlations, the study's setting on the Ingest tab, %s.",
    sieve_retention_basis_label(sieve_study_basis(state, types)), why
  )
}
#' Capture an engine call and what it printed
#'
#' `sieve_capture()` with the engine's printed progress and summary lines
#' collected instead of written to the console.
#'
#' @param expr An expression to evaluate.
#' @return The list from `sieve_capture()`, with `printed` added.
#' @author Ken Kelley
#' @noRd
sieve_capture_quiet <- function(expr) {
  run <- NULL
  printed <- utils::capture.output(run <- sieve_capture(expr))
  run$printed <- printed
  run
}

#' The largest number of factors the criteria probe
#'
#' @param max_k The requested maximum.
#' @param p The number of items.
#' @return An integer between 2 and `p - 1` (1 when `p` is 2).
#' @author Ken Kelley
#' @noRd
sieve_retention_max_k <- function(max_k, p) {
  max_k <- as.integer(sieve_finite_or(max_k, 8L))
  max(1L, min(max(2L, max_k), as.integer(p) - 1L))
}

#' Degrees of freedom of the unrestricted k factor model
#'
#' An exploratory (unrestricted) model with `k` factors for `p` items has
#' `((p - k)^2 - (p + k)) / 2` degrees of freedom: the `p (p - 1) / 2`
#' correlations less the `p k - k (k - 1) / 2` loadings that remain free
#' once the rotational freedom is removed (Lawley and Maxwell, 1971). A
#' negative value means more free parameters than correlations: the
#' solution is not unique, so nothing computed from its loadings is evidence
#' about the items.
#'
#' @param p Number of items.
#' @param k Number of factors (a vector is allowed).
#' @return The degrees of freedom, at full precision.
#' @author Ken Kelley
#' @noRd
sieve_efa_df <- function(p, k) {
  ((p - k)^2 - (p + k)) / 2
}

#' The largest number of factors an exploratory model identifies
#'
#' The largest `k` below `p` for which `sieve_efa_df()` is not negative.
#' The degrees of freedom fall as `k` rises, so every smaller `k` passes the
#' same count. The count is a necessary condition, not a guarantee that a
#' particular solution is proper.
#'
#' @param p Number of items.
#' @return An integer; 0 when not even one factor passes the count.
#' @author Ken Kelley
#' @noRd
sieve_max_identified_k <- function(p) {
  p <- as.integer(p)
  k <- seq_len(max(0L, p - 1L))
  ok <- k[sieve_efa_df(p, k) >= 0]
  if (length(ok)) max(ok) else 0L
}

#' The number of factors a Very Simple Structure series points to
#'
#' The `k` with the largest fit among the `k` that can be compared: those
#' whose exploratory model is identified (see `sieve_efa_df()`) and at which
#' the complexity is defined (a pattern that keeps two loadings per item
#' needs at least two factors; there is no complexity 2 fit with one
#' factor). With fewer than two such
#' `k` there is nothing to compare and no count is read.
#'
#' @param fit The fit values, one per `k` from 1 on.
#' @param identified Logical, one per `k`: is the `k` factor model
#'   identified?
#' @param complexity 1 or 2.
#' @return A list with `k` (the count, or `NA`), `candidates` (the `k`
#'   compared), and `unrestricted` (the `k` with the largest fit when the
#'   identification of the model is ignored, or `NA`).
#' @author Ken Kelley
#' @noRd
sieve_vss_count <- function(fit, identified, complexity = 1L) {
  k <- seq_along(fit)
  defined <- k >= complexity & is.finite(fit)
  cand <- k[defined & identified]
  free <- k[defined]
  list(
    k = if (length(cand) < 2L) NA_integer_ else as.integer(cand[which.max(fit[cand])]),
    candidates = as.integer(cand),
    unrestricted = if (length(free)) as.integer(free[which.max(fit[free])]) else NA_integer_
  )
}

#' Compute a result once per identity of its inputs
#'
#' A reactive expression is evaluated again whenever anything it read is
#' assigned, including an assignment that changes nothing the analysis uses
#' (an item's display label is part of the ledger, and so are its
#' descriptive summaries). For an analysis that is slow, or that draws a
#' seed, evaluating again is itself an error: a Monte Carlo reference would
#' be rerun, and a seed that was not supplied would be drawn anew, for the
#' same data. This helper keeps the last result of a named computation
#' together with the identity of the inputs it was computed from, and
#' computes again only when that identity changes.
#'
#' @param store An environment owned by the caller (one per module session).
#' @param slot Name of the computation.
#' @param key The identity of the inputs: any R object, compared with
#'   [identical()].
#' @param compute A function of no arguments that returns the result.
#' @return The kept result when `key` is the one it was computed from,
#'   otherwise the new result (which is then kept).
#' @author Ken Kelley
#' @noRd
sieve_keep <- function(store, slot, key, compute) {
  held <- store[[slot]]
  if (!is.null(held) && identical(held$key, key)) {
    return(held$value)
  }
  value <- compute()
  assign(slot, list(key = key, value = value), envir = store)
  value
}

#' Keep one value for every identity of the inputs seen in the session
#'
#' `sieve_keep()` keeps the value of the last identity only. The seed drawn
#' for the random parts of an analysis (when none is supplied) has to be
#' kept for every identity: inputs that return to an identity seen before
#' (respondents restored after a removal, an item returned to its scoring,
#' a rotation chosen again) must give the result computed for it then, to
#' the last digit, and a seed drawn anew would give other reference
#' datasets, other rotation starts, and so other values (or the same
#' factors in another column order). The most recent `most` identities are
#' kept.
#'
#' @param store An environment owned by the caller (one per module session).
#' @param slot Name of the value.
#' @param key The identity of the inputs (any R object; compared by its
#'   content hash).
#' @param compute A function of no arguments that returns the value.
#' @param most How many identities to keep.
#' @return The value kept for `key`, or the new value (which is then kept).
#' @author Ken Kelley
#' @noRd
sieve_keep_each <- function(store, slot, key, compute, most = 64L) {
  held <- store[[slot]]
  if (!is.list(held)) held <- list()
  id <- sieve_hash(key)
  if (!is.null(held[[id]])) {
    return(held[[id]])
  }
  value <- compute()
  held[[id]] <- value
  if (length(held) > most) held <- held[seq.int(length(held) - most + 1L, length(held))]
  assign(slot, held, envir = store)
  value
}

#' What the response categories of a set of items look like
#'
#' Polychoric and tetrachoric estimation fails, or becomes unstable, mostly
#' because of how the responses are distributed over the categories: a
#' category almost nobody used, or so many distinct values that the item is
#' not categorical in any useful sense. When the engine stops, its message
#' rarely says which. This states the two facts that bear on it, without a
#' threshold and without a diagnosis: how many distinct values the items
#' take, and how often the least used value occurs.
#'
#' @param X A data frame of complete numeric item responses.
#' @param items The columns to describe.
#' @return A sentence, or `NULL` when there is nothing to describe.
#' @author Ken Kelley
#' @noRd
sieve_category_facts <- function(X, items = names(X)) {
  X <- as.data.frame(X)[, intersect(items, names(X)), drop = FALSE]
  if (!ncol(X) || !nrow(X)) {
    return(NULL)
  }
  counts <- lapply(X, function(v) table(v))
  n_values <- vapply(counts, length, integer(1))
  least <- vapply(counts, function(t) as.integer(min(t)), integer(1))
  at <- which.min(least)
  value <- names(counts[[at]])[which.min(counts[[at]])]
  sprintf(
    "What the data show: %s per item, and the least used value (%s, on item %s) was observed %s among the %d complete cases.",
    if (min(n_values) == max(n_values)) {
      sprintf("%d distinct response values", min(n_values))
    } else {
      sprintf("between %d and %d distinct response values", min(n_values), max(n_values))
    },
    value, names(X)[at], if (least[at] == 1L) "once" else sprintf("%d times", least[at]), nrow(X)
  )
}

#' The Correlation Matrix the Retention Criteria Are Computed From
#'
#' Computes the requested correlations once, so every criterion that uses a
#' correlation matrix uses the same one. When the requested correlations
#' cannot be computed, Pearson correlations are computed instead and the
#' result says so: `used` differs from `requested`, `changed` is `TRUE`,
#' and `reason` holds what the engine reported. Timmerman and Lorenzo-Seva
#' (2011) describe the same explicit alternative for parallel analysis when
#' polychoric estimation does not converge. Warnings and messages the engine
#' raised when it did run (for example cells adjusted for zero counts) are
#' kept. The matrix is computed on one worker whatever the session's
#' `mc.cores` option is, so psych's internal pairwise parallel code cannot
#' lose conditions before they reach the outer supervised socket worker;
#' the values do not depend on the number of workers.
#'
#' Under `missing = "listwise"` every row with a missing item response is
#' set aside before any correlation is computed, whatever the basis, so
#' every entry of the matrix comes from the same rows and `n` is the number
#' of those rows; a message says how many rows were set aside. The
#' correlations of `p` items need at least `p + 1` such rows: with fewer
#' the sample matrix is singular (its rank is at most `n - 1`), and the
#' maximum likelihood factor analyses behind the criteria need a positive
#' definite matrix, so the result then fails with that reason. When the
#' matrix has undefined entries, the reason names the items that take a
#' single value in the rows used.
#'
#' A `requested`, `missing`, `types` or `aux` that is not one of the values
#' described below is refused with an error that names it; nothing is
#' substituted for it.
#'
#' @param X A data frame of numeric item responses (and the auxiliary
#'   columns named in `aux`). Under `missing = "listwise"` a row with any
#'   missing item response is set aside; `n`, `n_rows` and `n_dropped`
#'   report it.
#' @param types `NULL` (every item continuous), or a character vector of
#'   the confirmed item types, one per item column (the columns of `X` not
#'   named in `aux`), each one of the levels SIEVER records:
#'   `"continuous"`, `"ordered"`, `"binary"`, `"constant"` or `"text"` (`NA`
#'   is read as continuous). The mixed basis reads `"ordered"` and
#'   `"binary"` items as categorical and every other item as continuous, so
#'   a confirmed continuous item is never reinterpreted as ordinal (or the
#'   reverse).
#' @param requested `"pearson"`, `"polychoric"`, `"tetrachoric"`, or `"mixed"`.
#'   Choose the basis explicitly here; [sieve_retention_evidence()] can resolve
#'   `cor = "auto"` from the declared item types.
#' @param missing `"listwise"` (the default: the complete rows) or
#'   `"fiml"` (saturated FIML, the EM covariance of every row, read as
#'   Pearson correlations). FIML applies to Pearson correlations only:
#'   `"fiml"` with another basis is refused. Under `"fiml"` with no missing
#'   item response the complete rows are every row.
#' @param aux `NULL`, or the names of continuous auxiliary columns of `X`,
#'   used by saturated FIML and set aside otherwise.
#' @return A list with `requested`, `used` (`NA` when no matrix could be
#'   computed), `changed`, `ok`, `reason`, `facts` (when the requested
#'   polychoric, tetrachoric, or mixed correlations could not be computed,
#'   a sentence from `sieve_category_facts()`; otherwise `NULL`),
#'   `warnings`, `messages`, `R` (the matrix, or `NULL`), `seconds` (the
#'   time the correlations that are used took to compute), `n` (the
#'   complete rows: the rows the correlations use under `"listwise"`, the
#'   complete-case reference N under `"fiml"`), `n_rows` (the rows of
#'   `X`), `n_dropped` (under `"listwise"`, the rows set aside), `p`,
#'   `items`, and `types`.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' basis <- sieve_retention_cor(items, rep("continuous", 9), requested = "pearson")
#' basis$used
#' round(basis$R[1:3, 1:3], 3)
#' @export
sieve_retention_cor <- function(X, types = NULL, requested = "pearson", missing = "listwise", aux = NULL) {
  # The value a caller gave, as it was given, for the refusals.
  given <- function(x) {
    if (is.null(x)) return("NULL")
    paste(deparse(x, width.cutoff = 500L, nlines = 1L), collapse = "")
  }
  bases <- names(sieve_retention_basis_labels())
  if (!is.character(requested) || length(requested) != 1L || is.na(requested) || !requested %in% bases) {
    stop(sprintf(paste0(
      "`requested` must be one of \"pearson\", \"polychoric\", \"tetrachoric\" or \"mixed\", spelled ",
      "in lower case; it was %s, and no other correlations were substituted. To have the item types ",
      "choose the correlations (\"auto\"), call sieve_retention_evidence(X, types, cor = \"auto\"), ",
      "which resolves the request and passes the basis it resolves to here."
    ), given(requested)), call. = FALSE)
  }
  if (!is.character(missing) || length(missing) != 1L || is.na(missing) || !missing %in% c("listwise", "fiml")) {
    stop(sprintf(paste0(
      "`missing` must be \"listwise\" or \"fiml\", spelled in lower case; it was %s. \"listwise\" sets ",
      "aside every row with a missing item response; \"fiml\" estimates the Pearson correlations of ",
      "continuous items by saturated FIML (EM) from every row."
    ), given(missing)), call. = FALSE)
  }
  if (identical(missing, "fiml") && !identical(requested, "pearson")) {
    stop(sprintf(paste0(
      "missing = \"fiml\" estimates Pearson correlations of continuous items by saturated FIML (EM); ",
      "%s correlations have no FIML route here, so they are computed from the complete rows. ",
      "Use missing = \"listwise\" with requested = \"%s\", or requested = \"pearson\" with FIML."
    ), sieve_retention_basis_label(requested), requested), call. = FALSE)
  }
  X <- as.data.frame(X)
  if (!is.null(aux)) {
    absent <- if (is.character(aux) && !anyNA(aux)) setdiff(aux, names(X)) else NULL
    if (!is.character(aux) || anyNA(aux) || length(absent)) {
      stop(sprintf(paste0(
        "`aux` must be NULL or the names of auxiliary columns of X; it was %s%s."
      ), given(aux), if (length(absent)) {
        sprintf(", and X has no column named %s", paste(sprintf("'%s'", absent), collapse = ", "))
      } else {
        ""
      }), call. = FALSE)
    }
  }
  X[] <- lapply(X, sieve_as_number)
  item_names <- setdiff(names(X), aux)
  if (!length(item_names)) {
    stop("X has no item columns (every column is named in `aux`), so there is nothing to correlate.", call. = FALSE)
  }
  if (!is.null(types)) {
    types <- as.character(types)
    if (length(types) != length(item_names)) {
      stop(sprintf(paste0(
        "`types` must give one measurement level per item column: X has %d item column%s, and %d ",
        "type%s %s given (%s)."
      ),
      length(item_names), if (length(item_names) == 1L) "" else "s", length(types),
      if (length(types) == 1L) "" else "s", if (length(types) == 1L) "was" else "were", given(types)
      ), call. = FALSE)
    }
    unknown <- unique(types[!is.na(types) & !types %in% sieve_type_levels()])
    if (length(unknown)) {
      stop(sprintf(paste0(
        "`types` holds %s, which %s not a measurement level. Each entry must be one of the levels ",
        "SIEVER records, \"continuous\", \"ordered\", \"binary\", \"constant\" or \"text\" (NA is read as ",
        "continuous); the correlations to compute are chosen with `requested`."
      ), paste(sprintf("\"%s\"", unknown), collapse = ", "), if (length(unknown) == 1L) "is" else "are"),
      call. = FALSE)
    }
  }
  if (identical(missing, "fiml") && identical(requested, "pearson") && anyNA(X[, item_names, drop = FALSE])) {
    started <- proc.time()[["elapsed"]]
    read <- sieve_capture(DMAR::cov_fiml(X, aux = aux))
    ok <- read$ok && isTRUE(read$value$converged)
    return(list(requested = requested, used = "pearson", changed = FALSE, ok = ok,
      reason = if (!ok) read$error %||% "The saturated FIML covariance did not converge.",
      facts = "EM covariance from available continuous responses; the complete-case N is the reference size of the exploratory methods, a benchmark rather than an effective N or a guaranteed conservative choice.",
      warnings = read$warnings, messages = read$messages,
      R = if (ok) stats::cov2cor(read$value$S) else NULL,
      seconds = proc.time()[["elapsed"]] - started,
      n = sum(stats::complete.cases(X[, item_names, drop = FALSE])), n_used = read$value$N,
      n_rows = nrow(X), n_dropped = NA_integer_,
      p = length(item_names), items = item_names, types = types, missing = "fiml", moments = read$value,
      aux = aux))
  }
  X <- X[, item_names, drop = FALSE]
  items <- names(X)
  p <- ncol(X)
  types <- if (is.null(types)) rep("continuous", p) else types
  types[is.na(types)] <- "continuous"
  # Listwise deletion: every correlation below is computed from the same
  # complete rows, whatever the basis (psych would otherwise use each
  # pair's available rows), and n is the number of those rows.
  n_rows <- nrow(X)
  keep <- stats::complete.cases(X)
  n_dropped <- sum(!keep)
  if (n_dropped) {
    X <- X[keep, , drop = FALSE]
    message(sprintf(
      paste0(
        "%d of %d rows have a missing item response and were set aside (listwise deletion); ",
        "the correlations use the remaining %d complete rows."
      ),
      n_dropped, n_rows, nrow(X)
    ))
  }
  out <- list(
    requested = requested, used = NA_character_, changed = FALSE, ok = FALSE,
    reason = NULL, facts = NULL, warnings = character(0), messages = character(0),
    R = NULL, seconds = NA_real_, n = nrow(X), n_rows = n_rows, n_dropped = n_dropped,
    p = p, items = items, types = types
  )
  if (nrow(X) < p + 1L) {
    out$reason <- sprintf(
      paste0(
        "The correlations of %d items need at least %d complete rows, and %s. With fewer the sample ",
        "correlation matrix is singular (its rank is at most n - 1), and the maximum likelihood factor ",
        "analyses behind the criteria need a positive definite matrix.%s"
      ),
      p, p + 1L,
      if (n_dropped) {
        sprintf("only %d of the %d rows have every item answered", nrow(X), n_rows)
      } else {
        sprintf("there %s %d", if (nrow(X) == 1L) "is" else "are", nrow(X))
      },
      if (n_dropped && identical(requested, "pearson") && !any(types %in% c("ordered", "binary"))) {
        " Use missing = \"fiml\", which lets every row contribute to the Pearson correlations of continuous items, or reduce the item pool."
      } else {
        " Reduce the item pool, or gather more complete responses."
      }
    )
    return(out)
  }
  attempt <- function(basis) {
    # Keep psych's internal pairwise correlation work serial. Its own
    # parallel workers need not relay every notice, such as an empty-cell
    # correction, to the outer supervised socket worker.
    run <- withr::with_options(list(mc.cores = 1L), sieve_capture_quiet(switch(basis,
      pearson = stats::cor(X),
      polychoric = psych::polychoric(X, correct = 0.5, progress = FALSE)$rho,
      tetrachoric = psych::tetrachoric(X, correct = 0.5)$rho,
      mixed = {
        cont <- items[!types %in% c("ordered", "binary")]
        poly <- items[types == "ordered"]
        dich <- items[types == "binary"]
        psych::mixedCor(X,
          c = if (length(cont)) cont else NULL,
          p = if (length(poly)) poly else NULL,
          d = if (length(dich)) dich else NULL,
          correct = 0.5
        )$rho
      }
    )))
    why <- NULL
    R <- run$value
    if (!run$ok) {
      why <- run$error
    } else if (!is.matrix(R) || nrow(R) != ncol(R)) {
      why <- "The engine did not return a square correlation matrix."
    } else if (ncol(R) != p || (!is.null(colnames(R)) && !all(items %in% colnames(R)))) {
      why <- sprintf(
        "The engine returned correlations for %d of the %d items%s.",
        ncol(R), p,
        if (length(run$messages)) paste0(" (", paste(trimws(run$messages), collapse = " "), ")") else ""
      )
    } else {
      if (!is.null(colnames(R))) {
        R <- R[items, items, drop = FALSE]
      }
      dimnames(R) <- list(items, items)
      if (!all(is.finite(R))) {
        # The cause is named: an item with one value in the rows used has
        # no variance, so its correlations are undefined.
        flat <- items[vapply(X, function(v) length(unique(v[!is.na(v)])) < 2L, logical(1))]
        why <- if (length(flat)) {
          sprintf(
            "%s %s %s a single value in the %d rows used, so %s correlations are undefined.",
            if (length(flat) == 1L) "Item" else "Items",
            paste(sprintf("'%s'", flat), collapse = ", "),
            if (length(flat) == 1L) "takes" else "take", nrow(X),
            if (length(flat) == 1L) "its" else "their"
          )
        } else {
          sprintf(
            "The engine returned undefined correlations for %s, although every item varies in the %d rows used.",
            paste(sprintf("'%s'", items[apply(!is.finite(R), 1L, any)]), collapse = ", "), nrow(X)
          )
        }
      }
    }
    list(
      R = if (is.null(why)) R else NULL, why = why,
      warnings = run$warnings, messages = run$messages
    )
  }

  # Each attempt is timed: the time of the correlations that are used is
  # what the parallel analysis pays again for every reference dataset.
  timed <- function(basis) {
    started <- proc.time()[["elapsed"]]
    res <- attempt(basis)
    res$seconds <- proc.time()[["elapsed"]] - started
    res
  }
  first <- timed(requested)
  if (is.null(first$why)) {
    out$used <- requested
    out$ok <- TRUE
    out$R <- first$R
    out$seconds <- first$seconds
    out$warnings <- first$warnings
    out$messages <- first$messages
    return(out)
  }
  if (identical(requested, "pearson")) {
    out$reason <- sprintf("Pearson correlations could not be computed: %s", first$why)
    out$warnings <- first$warnings
    out$messages <- first$messages
    return(out)
  }
  second <- timed("pearson")
  label <- sieve_retention_basis_label(requested)
  # The engine's own message is often terse, so what the response
  # categories look like is stated next to it, as a fact about the data.
  out$facts <- sieve_category_facts(
    X, if (identical(requested, "mixed")) items[types %in% c("ordered", "binary")] else items
  )
  if (is.null(second$why)) {
    out$used <- "pearson"
    out$changed <- TRUE
    out$ok <- TRUE
    out$R <- second$R
    out$seconds <- second$seconds
    out$reason <- sprintf(
      "%s correlations could not be computed: %s",
      paste0(toupper(substring(label, 1L, 1L)), substring(label, 2L)),
      first$why
    )
    out$warnings <- unique(c(first$warnings, second$warnings))
    out$messages <- unique(c(first$messages, second$messages))
    return(out)
  }
  out$reason <- sprintf(
    "%s correlations could not be computed (%s), and neither could Pearson correlations (%s).",
    paste0(toupper(substring(label, 1L, 1L)), substring(label, 2L)),
    first$why, second$why
  )
  out$warnings <- unique(c(first$warnings, second$warnings))
  out$messages <- unique(c(first$messages, second$messages))
  out
}

#' One parallel analysis reference series
#'
#' @param draws The matrix of reference eigenvalues (one row per usable
#'   random dataset, one column per rank), or `NULL`.
#' @param mean The engine's reported mean of those draws.
#' @param reason Why the reference is not available, when it is not.
#' @param source A plain description of where the reference data came from.
#' @return A list with `available`, `reason`, `source`, `draws`, `mean`, and
#'   `p95` (the 95th percentile of each column of `draws`, R's type 7
#'   quantile, which is DMAR's).
#' @author Ken Kelley
#' @noRd
sieve_pa_reference <- function(draws = NULL, mean = NULL, reason = NULL, source = NULL) {
  ok <- is.matrix(draws) && nrow(draws) > 0L && length(mean) == ncol(draws) && all(is.finite(draws)) &&
    isTRUE(all.equal(unname(colMeans(draws)), unname(as.numeric(mean)), tolerance = 1e-8))
  if (!ok) {
    return(list(
      available = FALSE,
      reason = reason %||% "The engine's eigenvalue draws are not laid out as expected, so no reference was read from them.",
      source = source, draws = NULL, mean = NULL, p95 = NULL
    ))
  }
  draws <- unname(draws)
  list(
    available = TRUE, reason = NULL, source = source,
    draws = draws,
    mean = unname(as.numeric(mean)),
    p95 = apply(draws, 2L, stats::quantile, probs = 0.95, names = FALSE, type = 7)
  )
}

#' How DMAR's parallel analysis reads the page's correlations
#'
#' Which call of [DMAR::parallel_analysis()] gives the parallel analysis of
#' the page's correlation basis. DMAR forms the correlations from the item
#' responses (`data`) for continuous items (Pearson correlations of the
#' complete cases, or the saturated FIML matrix of [DMAR::cov_fiml()], the
#' page's own two bases) and for a pool whose every item is ordered or
#' binary (polychoric correlations through `lavaan::lavCor()`, the same
#' two-step estimator as the page's psych matrix; the two differ in the
#' optimizer and in psych's 0.5 added to empty cells of larger tables). On
#' that route both kinds of reference data are run: independent normal
#' responses (`"normal"`; categorized at each item's observed category
#' proportions when the items are ordered) and each item's own responses
#' resampled (`"resample"`), each analyzed with the same correlations as the
#' items. Where the raw-data route does not form the page's matrix (mixed
#' correlations, which DMAR's ordered interface does not take, or a FIML
#' matrix with auxiliary variables, which DMAR's parallel analysis does not
#' take), the matrix is passed as `S` with its reference size `N`, and DMAR
#' then offers the normal reference analyzed with Pearson correlations
#' alone.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @return A list with `form` (`"data"` or `"matrix"`), `missing`
#'   (`"listwise"` or `"fiml"` on the data route, `NULL` on the matrix
#'   route), `ordered` (`TRUE` when DMAR reads every item as ordered),
#'   `families` (DMAR's references, `"normal"` and on the data route
#'   `"resample"`), `correlations` (what the reference datasets are analyzed
#'   with: `"pearson"` or `"polychoric"`), and `why` (on the matrix route,
#'   why the matrix is passed).
#' @author Ken Kelley
#' @noRd
sieve_pa_route <- function(basis) {
  used <- basis$used %||% "pearson"
  data_route <- function(missing, ordered) {
    list(form = "data", missing = missing, ordered = ordered, families = c("normal", "resample"),
      correlations = if (ordered) "polychoric" else "pearson", why = NULL)
  }
  matrix_route <- function(why) {
    list(form = "matrix", missing = NULL, ordered = FALSE, families = "normal",
      correlations = "pearson", why = why)
  }
  if (identical(used, "pearson")) {
    if (!identical(basis$missing, "fiml")) return(data_route("listwise", FALSE))
    if (length(basis$aux)) {
      return(matrix_route(paste(
        "the saturated FIML matrix uses auxiliary variables, which DMAR::parallel_analysis() does not take,",
        "so the page's matrix is passed with its complete-case reference N"
      )))
    }
    return(data_route("fiml", FALSE))
  }
  if (used %in% c("polychoric", "tetrachoric")) return(data_route("listwise", TRUE))
  matrix_route(paste(
    "DMAR::parallel_analysis() reads ordered items only when every item is ordered,",
    "so the page's mixed matrix is passed with its complete-case reference N"
  ))
}

#' The calls of a parallel analysis to DMAR
#'
#' One [DMAR::parallel_analysis()] call per reference (see
#' `sieve_pa_route()`), every argument written out, each from the run's
#' recorded seed: DMAR sets the seed locally and restores the caller's
#' stream, so each reference is a function of the seed alone and replays
#' in any order.
#'
#' @param X The item data the page analyzes (complete cases, or the item
#'   responses with their missing values on the FIML route).
#' @param basis The result of `sieve_retention_cor()` for `X`.
#' @param n_iter The number of reference datasets (`B`).
#' @param seed The run's seed (an integer).
#' @return A list with `fun` (`"parallel_analysis"`), `route`, and `calls`
#'   (a named list, one call per reference, each a list of `fun` and
#'   `args`).
#' @author Ken Kelley
#' @noRd
sieve_pa_dmar_calls <- function(X, basis, n_iter, seed) {
  request <- sieve_dmar_boot_request(n_iter, conf_level = 0.95, sidedness = "one")
  if (!request$ok) stop(request$message, call. = FALSE)
  route <- sieve_pa_route(basis)
  common <- if (identical(route$form, "data")) {
    X <- as.data.frame(X)
    items <- intersect(basis$items %||% names(X), names(X))
    a <- list(data = X[, items, drop = FALSE], missing = route$missing)
    if (isTRUE(route$ordered)) a$ordered <- TRUE
    a
  } else {
    list(S = basis$R, N = as.integer(basis$n))
  }
  calls <- lapply(route$families, function(family) {
    list(fun = "parallel_analysis", args = c(common, list(
      reference = family, B = as.integer(n_iter), percentile = 0.95, conf_level = 0.95,
      seed = as.integer(seed)
    )))
  })
  names(calls) <- route$families
  list(fun = "parallel_analysis", route = route, calls = calls)
}

#' Run the calls of a parallel analysis
#'
#' Each reference's call through `sieve_efa_dmar_run()`, which keeps
#' DMAR's warnings, messages, and an error's condition classes and reason.
#' This is what the background worker runs (`sieve_efa_dmar_start()` with
#' this runner) and what an in-session run calls.
#'
#' @param call The result of `sieve_pa_dmar_calls()`.
#' @param rng_kind The [RNGkind()] settings of the process that drew the
#'   seed.
#' @return A record: `value` (one record of `sieve_efa_dmar_run()` per
#'   reference), `ok` (`TRUE` when at least one reference ran), `error`,
#'   `error_class` and `reason` (of the first reference that failed, when
#'   none ran), `warnings`, `messages`, `seconds`, and `fun`.
#' @author Ken Kelley
#' @noRd
sieve_pa_dmar_run <- function(call, rng_kind = NULL) {
  started <- proc.time()[["elapsed"]]
  records <- lapply(call$calls, sieve_efa_dmar_run, rng_kind = rng_kind)
  ok <- vapply(records, function(r) isTRUE(r$ok), logical(1))
  bad <- if (!any(ok) && length(records)) records[[1L]] else NULL
  list(
    value = records, ok = any(ok),
    error = bad$error, error_class = bad$error_class, reason = bad$reason,
    warnings = unique(unlist(lapply(records, `[[`, "warnings"))),
    messages = unique(unlist(lapply(records, `[[`, "messages"))),
    seconds = proc.time()[["elapsed"]] - started, fun = "parallel_analysis"
  )
}

#' Where a reference came from, in words
#'
#' @param family DMAR's reference (`"normal"` or `"resample"`).
#' @param route The result of `sieve_pa_route()`.
#' @return One phrase.
#' @author Ken Kelley
#' @noRd
sieve_pa_source_words <- function(family, route) {
  if (identical(family, "resample")) {
    return(sprintf("each item's observed responses resampled independently (%s correlations)",
      if (isTRUE(route$ordered)) "polychoric" else "Pearson"))
  }
  if (isTRUE(route$ordered)) {
    return(paste(
      "independent normal responses categorized at each item's observed category proportions",
      "(polychoric correlations)"
    ))
  }
  "independent normal responses (Pearson correlations)"
}

#' What a parallel analysis's observed eigenvalues were computed from
#'
#' @param pa The result of `sieve_parallel_analysis()`.
#' @return One phrase.
#' @author Ken Kelley
#' @noRd
sieve_pa_matrix_words <- function(pa) {
  route <- pa$route %||% sieve_pa_route(list(used = pa$basis_used))
  label <- sieve_retention_basis_label(pa$basis_used)
  if (identical(route$form, "matrix")) {
    return(sprintf("page's %s correlation matrix, passed to DMAR as S with N = %d", label, as.integer(pa$n)))
  }
  if (isTRUE(route$ordered)) {
    return(sprintf(
      "%s correlations DMAR computes from the %d complete cases (lavaan::lavCor, the two-step estimator of the page's psych matrix)",
      label, as.integer(pa$n)
    ))
  }
  if (identical(route$missing, "fiml")) {
    return(sprintf(
      "saturated FIML correlations DMAR computes from every observed value (DMAR::cov_fiml), with the complete-case N = %d as the reference size",
      as.integer(pa$n)
    ))
  }
  sprintf("%s correlations of the %d complete cases", label, as.integer(pa$n))
}

#' Read one of DMAR's references into the page's record
#'
#' @param table The `dmar_parallel_analysis` table, or `NULL` when the call
#'   failed.
#' @param record The call's record from `sieve_efa_dmar_run()`.
#' @param family `"normal"` or `"resample"`.
#' @param route The result of `sieve_pa_route()`.
#' @return A reference (see `sieve_pa_reference()`) with DMAR's Monte Carlo
#'   limits of the percentile (`mc_lower`, `mc_upper`), its `counts`
#'   (attempted, usable, failed, boundary reference fits), `n_used`,
#'   `family`, and `correlations`.
#' @author Ken Kelley
#' @noRd
sieve_pa_dmar_reference <- function(table, record, family, route) {
  source <- sieve_pa_source_words(family, route)
  if (is.null(table)) {
    return(sieve_pa_reference(reason = sprintf("DMAR::parallel_analysis() did not run this reference: %s",
      sieve_efa_dmar_refusal_text(record)), source = source))
  }
  sieve_dmar_require_columns(table, c("reference_mean", "reference_quantile",
    "reference_quantile_ci_lower", "reference_quantile_ci_upper"), "Parallel analysis")
  counts <- attr(table, "counts", exact = TRUE)
  draws <- attr(table, "reference_draws", exact = TRUE)
  usable <- draws[stats::complete.cases(draws), , drop = FALSE]
  if (!all(is.finite(table$reference_quantile))) {
    out <- sieve_pa_reference(reason = sprintf(
      paste0(
        "%d of the %d reference datasets returned defined eigenvalues; DMAR withholds the reference ",
        "and the count when fewer than half, or fewer than two, are usable."
      ),
      as.integer(counts$usable), as.integer(counts$attempted)
    ), source = source)
    out$counts <- counts
    return(out)
  }
  out <- sieve_pa_reference(usable, table$reference_mean, source = source)
  if (!isTRUE(out$available)) return(out)
  out$p95 <- unname(as.numeric(table$reference_quantile))
  out$mc_lower <- unname(as.numeric(table$reference_quantile_ci_lower))
  out$mc_upper <- unname(as.numeric(table$reference_quantile_ci_upper))
  out$counts <- counts
  out$n_used <- as.integer(counts$usable)
  out$family <- family
  out$correlations <- if (identical(family, "resample") || isTRUE(route$ordered)) route$correlations else "pearson"
  out
}

#' The empirical factor-analytic eigenvalues
#'
#' The eigenvalues of the correlation matrix reduced by the communalities
#' of an `nfactors`-factor maximum likelihood solution: the diagonal of
#' ones is replaced by the communalities and the eigenvalues of that
#' matrix are taken. The solution is the one [DMAR::parallel_analysis()]
#' fits for its fixed reduction, `stats::factanal()` with its lower bound
#' of 0.005 on every uniqueness, so with one factor the series equals the
#' observed series of DMAR's parallel analysis of the same matrix.
#'
#' A uniqueness at that bound is a boundary, improper (Heywood) solution:
#' the item's communality is held at 0.995 rather than estimated. Such
#' items are named in `improper`.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param nfactors The number of factors whose communalities reduce the
#'   matrix (1, the fixed reduction of the parallel analysis).
#' @return A list with `values` (or `NULL`), `improper` (the items whose
#'   uniqueness is at the lower bound; empty when the solution is proper),
#'   `warnings`, `messages`, and `error`.
#' @author Ken Kelley
#' @noRd
sieve_pa_empirical <- function(basis, nfactors = 1L) {
  R <- basis$R
  run <- sieve_capture_quiet({
    if (!is.matrix(R) || any(!is.finite(R)) ||
      min(eigen(R, symmetric = TRUE, only.values = TRUE)$values) <= 0) {
      stop("The analysis correlation matrix is not positive definite.", call. = FALSE)
    }
    stats::factanal(covmat = list(cov = R, n.obs = max(3L, as.integer(basis$n %||% 3L))),
      factors = as.integer(nfactors), rotation = "none")
  })
  u2 <- if (run$ok) run$value$uniquenesses else NULL
  values <- if (is.numeric(u2)) {
    reduced <- R
    diag(reduced) <- 1 - u2
    eigen(reduced, symmetric = TRUE, only.values = TRUE)$values
  }
  floor_u2 <- 0.005
  improper <- if (is.numeric(u2)) {
    at <- which(u2 <= floor_u2 * (1 + 1e-3))
    if (is.null(names(u2))) as.character(at) else names(u2)[at]
  } else {
    character(0)
  }
  list(
    values = values, improper = improper, warnings = run$warnings, messages = run$messages,
    error = if (is.null(values)) run$error %||% "The engine returned no factor-analytic eigenvalues." else NULL
  )
}

#' The sentence that says a scree plot's reduced series rests on an
#' improper solution
#'
#' @param values The result of `sieve_scree_values()`.
#' @return A sentence, or `NULL` when the solution is proper.
#' @author Ken Kelley
#' @noRd
sieve_scree_improper_text <- function(values) {
  bad <- values$improper %||% character(0)
  if (!length(bad)) {
    return(NULL)
  }
  one <- length(bad) == 1L
  sprintf(
    paste(
      "Improper solution (Heywood case): in the %s solution the %s of %s %s at the lower bound,",
      "so %s and the reduced eigenvalues computed with %s should not be read; a smaller number",
      "of factors extracted may give a proper solution."
    ),
    sieve_scree_factor_words(values$nfactors %||% 1L),
    if (one) "uniqueness" else "uniquenesses", sieve_name_some(bad), if (one) "is" else "are",
    if (one) "that item's communality" else "those items' communalities",
    if (one) "it" else "them"
  )
}

#' Assemble a parallel analysis from DMAR's references
#'
#' Reads the observed series and each reference from DMAR's tables: the
#' mean (Horn, 1965) and the 95th percentile (Glorfeld, 1995) of the usable
#' reference eigenvalues, DMAR's Monte Carlo limits of the percentile, and
#' DMAR's counts of usable, failed and boundary reference fits. The
#' references keep the page's names: `simulated` is DMAR's `"normal"`
#' reference and `resampled` its `"resample"` reference. The measured time
#' per reference dataset is recorded for the next estimate.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param call The result of `sieve_pa_dmar_calls()`.
#' @param record The result of `sieve_pa_dmar_run()`.
#' @param rng The run's seed record.
#' @param n_iter The number of reference datasets requested.
#' @param seconds The time the run took.
#' @return The list described at `sieve_parallel_analysis()`.
#' @author Ken Kelley
#' @noRd
sieve_pa_dmar_result <- function(basis, call, record, rng, n_iter, seconds = NA_real_) {
  route <- call$route
  none <- function(why) sieve_pa_reference(reason = why)
  out <- list(
    ok = FALSE, status = "failed", reason = NULL, error_class = NULL,
    warnings = record$warnings %||% character(0), messages = record$messages %||% character(0),
    path = route$form, route = route, call = call, engine_name = "DMAR::parallel_analysis",
    basis_requested = basis$requested, basis_used = basis$used,
    n_iter = 0L, n_iter_requested = as.integer(n_iter), n = basis$n, p = basis$p, rng = rng,
    cores = 1L, seconds = seconds, failed = 0L, failed_reasons = character(0),
    empirical = NULL, boundary = character(0),
    reference = list(
      simulated = none("Parallel analysis did not run."),
      resampled = none("Parallel analysis did not run.")
    ),
    dmar = list(), engine = NULL
  )
  records <- record$value %||% list()
  tables <- lapply(records, function(r) if (isTRUE(r$ok)) r$value)
  ran <- Filter(Negate(is.null), tables)
  if (!isTRUE(record$ok) || !length(ran)) {
    first <- if (length(records)) records[[1L]] else record
    out$error_class <- first$error_class
    out$reason <- sprintf("DMAR::parallel_analysis() did not run: %s", sieve_efa_dmar_refusal_text(first))
    return(out)
  }
  lead <- ran[[1L]]
  method <- attr(lead, "method", exact = TRUE)
  out$ok <- TRUE
  out$status <- "ran"
  out$dmar <- ran
  out$empirical <- unname(as.numeric(lead$estimate))
  out$n <- as.integer(method$N)
  out$benchmark <- isTRUE(method$reference_is_benchmark)
  out$boundary <- attr(lead, "observed_boundary", exact = TRUE) %||% character(0)
  out$reference$simulated <- sieve_pa_dmar_reference(tables$normal, records$normal, "normal", route)
  out$reference$resampled <- if ("resample" %in% route$families) {
    sieve_pa_dmar_reference(tables$resample, records$resample, "resample", route)
  } else {
    none(paste0(
      "A resampled reference is not offered on this basis: ", route$why %||% "the matrix is passed to DMAR",
      ", and DMAR's matrix route takes the normal reference alone."
    ))
  }
  counts <- lapply(ran, attr, which = "counts", exact = TRUE)
  out$n_iter <- as.integer(max(vapply(counts, function(cnt) as.numeric(cnt$usable), numeric(1))))
  out$failed <- as.integer(sum(vapply(counts, function(cnt) as.numeric(cnt$failed), numeric(1))))
  out$failed_reasons <- unique(unlist(lapply(counts, function(cnt) names(cnt$failure_reasons))))
  # The observed series and the reference means under the names the
  # figure's object carries (fa.sim: the normal reference's mean; fa.simr:
  # the resampled reference's; both means, Horn's rule).
  out$engine <- list(
    fa.values = out$empirical,
    fa.sim = out$reference$simulated$mean,
    fa.simr = out$reference$resampled$mean
  )
  # The measured rate: the run's seconds less the FIML matrices it
  # estimated again (counted at the basis's own time, sieve_pa_cost()),
  # over the units of the calls that ran.
  spent <- record$seconds
  if (length(spent) == 1L && is.finite(spent)) {
    cost <- sieve_pa_cost(basis, n_iter)
    share <- length(ran) / cost$calls
    sieve_rate_note(cost$kind, spent - share * cost$fixed, share * cost$work)
  }
  out
}

#' The units of work of one reference dataset
#'
#' Measured on one machine (one process, DMAR 1.1.0, both kinds of reference
#' data, 300 to 30,000 cases and 6 to 60 items). A continuous reference
#' dataset costs a fixed part (the one-factor reduction of its matrix, about
#' the square of the item count) and a part that grows with the cases: the
#' normal draws or the resampled responses (cases times items) and the
#' correlations (cases times the square of the items). The fitted model,
#' 0.0009 + 2.6e-6 p^2 + N (2.8e-8 p + 4.3e-10 p^2) seconds, put the 32
#' measurements at 0.80 to 1.37 times its values: 0.0009 seconds at 6 items
#' and 300 cases, 0.016 at 30 items and 10,000 cases, 0.04 at 30 items and
#' 30,000 cases, 0.11 at 60 items and 30,000 cases. A polychoric reference
#' dataset (lavaan's two-step estimates, `lavaan::lavCor()`) costs 0.015 +
#' 3.2e-4 per item pair + 2.1e-6 N p seconds, with 19 measurements at 0.93
#' to 1.10 times its values (0.023 seconds at 6 items and 300 cases, 0.11 at
#' 24 items and 300, 0.25 at 24 items and 3,000, 0.29 at 12 items and
#' 10,000). One unit is 0.001 seconds of a continuous dataset or 0.03 of a
#' polychoric one (`sieve_rate_defaults()`), so a rate measured in this
#' session rescales the whole model.
#'
#' @param p The number of items.
#' @param correlations `"pearson"` or `"polychoric"`.
#' @param n The reference sample size of each dataset (DMAR's `N`, the
#'   page's complete cases).
#' @return The units of one reference dataset.
#' @author Ken Kelley
#' @noRd
sieve_pa_units <- function(p, correlations = "pearson", n) {
  p <- as.numeric(p)
  n <- max(0, sieve_finite_or(as.numeric(n), 0))
  if (identical(correlations, "polychoric")) {
    0.5 + sieve_pairs(p) / 95 + n * p / 14300
  } else {
    1 + p^2 / 400 + n * p / 36000 + n * p^2 / 2.4e6
  }
}

#' The work of a parallel analysis before it runs
#'
#' Each DMAR call computes the observed matrix and its reduction once
#' (about one reference dataset's work) before its reference datasets, so
#' a run of `n_iter` datasets for each of its calls is `n_iter + 1` dataset
#' units per call. On the saturated FIML route each call also estimates the
#' FIML matrix of the item responses again (`DMAR::cov_fiml()`, the
#' computation the page's basis timed, `basis$seconds`): at 30 items and
#' 10,000 cases with 5% of each item missing that took 13 seconds a call,
#' more than the 100 reference datasets that followed, so it is counted
#' at the time the basis measured, once per call, not as a rate.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param n_iter The number of reference datasets for each call.
#' @return A list with `route`, `kind` (the rate's name), `units` (of one
#'   reference dataset), `calls`, `draws` (every reference dataset of the
#'   run), `work` (units of the whole run), and `fixed` (seconds measured,
#'   not rated: the FIML matrix of every call).
#' @author Ken Kelley
#' @noRd
sieve_pa_cost <- function(basis, n_iter) {
  route <- sieve_pa_route(basis)
  units <- sieve_pa_units(basis$p %||% 3L, route$correlations, basis$n)
  calls <- length(route$families)
  draws <- as.integer(n_iter) * as.integer(calls)
  fiml <- identical(route$form, "data") && identical(route$missing, "fiml")
  fixed <- if (fiml) calls * max(0, sieve_finite_or(as.numeric(basis$seconds), 0)) else 0
  list(
    route = route, kind = paste0("pa_dmar_", route$correlations), units = units,
    calls = calls, draws = draws, work = (draws + calls) * units, fixed = fixed
  )
}

#' The seconds a background R process takes to start
#'
#' A background worker loads R, lavaan, DMAR and SIEVER before it computes
#' anything: about two seconds, as measured.
#'
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_pa_start_seconds <- function() 2

#' The plan of a parallel analysis before it runs
#'
#' How long the run would take, stated before it starts, so the researcher
#' decides whether to wait. The time per reference dataset is the rate an
#' earlier run measured in this process, or the standard rate, times the
#' units of one dataset at this many items and cases (`sieve_pa_units()`),
#' for every reference dataset of every call and each call's observed
#' matrix (two references on the data route, one on the matrix route),
#' plus, on the saturated FIML route, each call's FIML matrix at the time
#' the page's basis took for it (`sieve_pa_cost()`). A first run estimated
#' at `threshold` seconds or less at the standard rates runs by itself, in
#' the session (the run policy, `sieve_runs_by_itself()`); a run on
#' request estimated above two seconds (`sieve_auto_seconds()`, about the
#' time a background process takes to start) runs in a background R
#' process, whose start is added to the wait.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param n_iter The number of reference datasets.
#' @param threshold Seconds above which the run waits to be asked for.
#' @return A list with `n_iter`, `families`, `draws` (every reference
#'   dataset of the run), `per_dataset` (seconds), `fixed` (the seconds of
#'   the FIML matrices DMAR estimates again, 0 off that route), `compute`
#'   (seconds of computation), `background` (`TRUE` when a run on request
#'   goes to a background process), `seconds` (the estimated wait),
#'   `label`, `source`, `automatic`, `route`, and `cores` (1: DMAR runs the
#'   references in one process).
#' @author Ken Kelley
#' @noRd
sieve_pa_plan <- function(basis, n_iter, threshold = sieve_auto_seconds()) {
  request <- sieve_dmar_boot_request(n_iter, conf_level = 0.95, sidedness = "one")
  if (!request$ok) {
    route <- sieve_pa_route(basis)
    return(list(n_iter = n_iter, families = route$families, draws = NA_integer_,
      per_dataset = NA_real_, fixed = 0, compute = 0, background = FALSE,
      seconds = 0, label = "not run", source = "DMAR request rule",
      automatic = FALSE, route = route, cores = 1L, cores_available = 1L,
      refused = request$message, required = request$required))
  }
  cost <- sieve_pa_cost(basis, n_iter)
  route <- cost$route
  kind <- cost$kind
  measured <- exists(kind, envir = sieve_rates, inherits = FALSE)
  per <- sieve_rate(kind) * cost$units
  compute <- sieve_rate(kind) * cost$work + cost$fixed
  standard <- unname(sieve_rate_defaults()[[kind]]) * cost$work + cost$fixed
  # A background process takes about two seconds to start: a run computed
  # in less goes faster in the session (the EFA page's rule).
  background <- is.finite(compute) && compute > sieve_auto_seconds()
  seconds <- compute + if (background) sieve_pa_start_seconds() else 0
  list(
    n_iter = n_iter, families = route$families, draws = cost$draws, per_dataset = per,
    fixed = cost$fixed, compute = compute, background = background, seconds = seconds,
    label = sieve_duration_label(seconds),
    source = if (measured) "the time per dataset an earlier run in this session measured" else "DMAR's standard rate on this kind of data",
    automatic = is.finite(standard) && standard <= threshold &&
      !(is.finite(compute) && compute > sieve_auto_slack() * threshold),
    route = route, cores = 1L, cores_available = 1L
  )
}

#' The number of processor cores this computer reports
#'
#' @return A single integer, or `NA` when R cannot tell.
#' @author Ken Kelley
#' @noRd
sieve_detected_cores <- function() {
  n <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  if (length(n) != 1L || !is.finite(n)) NA_integer_ else as.integer(n)
}

#' The help line under the Dimensionality tab's Workers control
#'
#' @return One sentence or two.
#' @author Ken Kelley
#' @noRd
sieve_workers_help <- function() {
  detected <- sieve_detected_cores()
  paste0(
    "Workers are separate R processes, each on its own processor core, that fit the exploratory ",
    "models of a requested model fit by k at the same time. ",
    if (identical(.Platform$OS.type, "windows")) {
      "On Windows the default is one; explicit requests use separate socket processes. "
    } else if (is.na(detected)) {
      "The default is all of this computer's cores but one, at most 8. "
    } else {
      sprintf(
        "This computer has %d core%s; the default is all but one, at most 8. ",
        detected, if (detected == 1L) "" else "s"
      )
    },
    "The result does not depend on the number. Parallel analysis does not use them: a run ",
    "estimated above two seconds runs in one background R process of its own, and a shorter ",
    "one in this session."
  )
}

#' The wait of a parallel analysis that has not run, in plain words
#'
#' @param plan The result of `sieve_pa_plan()`.
#' @return One paragraph: what the run would do, how long each reference
#'   dataset takes, the estimated wait, where it runs, and the button that
#'   starts it.
#' @author Ken Kelley
#' @noRd
sieve_pa_wait_sentence <- function(plan) {
  if (!is.null(plan$refused)) return(plan$refused)
  per <- plan$per_dataset
  each <- if (!is.finite(per)) {
    "an unknown time each"
  } else if (per < 1) {
    sprintf("about %.3f seconds each", per)
  } else if (per < 90) {
    sprintf("about %.0f seconds each", per)
  } else {
    sprintf("%s each", sieve_duration_label(per))
  }
  refs <- if (length(plan$families) > 1L) {
    sprintf("%d reference datasets for each of its two kinds of reference data (%d in all)", plan$n_iter, plan$draws)
  } else {
    sprintf("%d reference datasets", plan$n_iter)
  }
  fiml <- sieve_finite_or(plan$fixed, 0)
  sprintf(
    paste0(
      "Parallel analysis has not run yet. DMAR::parallel_analysis() would draw %s; at %s%s, ",
      "the estimated wait is %s%s. Press Run parallel analysis to start it."
    ),
    refs, each,
    if (fiml > 0.05) {
      sprintf(
        paste0(
          ", plus %s in all for DMAR to estimate the saturated FIML matrix again, once in each of ",
          "its %s (each time as long as this page's matrix took)"
        ),
        sieve_duration_label(fiml), if (length(plan$families) > 1L) "two calls" else "call"
      )
    } else {
      ""
    },
    plan$label,
    if (isTRUE(plan$background)) ", in a background R process (its start included) while the page stays usable" else ""
  )
}

#' A parallel analysis that has not been run
#'
#' The record the Dimensionality tab holds while a run that would take
#' longer than the threshold waits for the researcher's button: nothing is
#' suggested, and the reason says what the wait would be and what to press.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param n_iter The number of reference datasets requested.
#' @param rng The seed record the run would use.
#' @param plan The result of `sieve_pa_plan()`.
#' @return A list in the shape of `sieve_parallel_analysis()`'s, with
#'   `status = "not run"`.
#' @author Ken Kelley
#' @noRd
sieve_pa_not_run <- function(basis, n_iter, rng, plan) {
  none <- function(why) sieve_pa_reference(reason = why)
  reason <- sieve_pa_wait_sentence(plan)
  route <- plan$route %||% sieve_pa_route(basis)
  list(
    ok = FALSE, status = "not run", reason = reason, error_class = NULL,
    warnings = character(0), messages = character(0), path = route$form, route = route, call = NULL,
    engine_name = "DMAR::parallel_analysis",
    basis_requested = basis$requested, basis_used = basis$used,
    n_iter = 0L, n_iter_requested = as.integer(n_iter), n = basis$n, p = basis$p, rng = rng,
    cores = 1L, seconds = NA_real_, failed = 0L, failed_reasons = character(0),
    empirical = NULL, boundary = character(0),
    reference = list(simulated = none(reason), resampled = none(reason)),
    dmar = list(), engine = NULL, plan = plan
  )
}

#' Factor-analytic parallel analysis with both reference series
#'
#' Horn's (1965) parallel analysis on factor-analytic eigenvalues by
#' [DMAR::parallel_analysis()]: the eigenvalues of the correlation matrix
#' reduced by the communalities of a fixed one-factor maximum likelihood
#' solution (`stats::factanal()`), against the same reduction of each
#' reference dataset, with the reference eigenvalues of every dataset kept,
#' so that the mean (Horn, 1965) and the 95th percentile (Glorfeld, 1995)
#' are read from one set of draws and the choice between them costs no new
#' simulation. DMAR reports the Monte Carlo limits of the percentile, and
#' counts the reference fits that are usable, failed, or at a boundary.
#'
#' Two kinds of reference data are kept apart because they are different
#' analyses (see `sieve_pa_route()` for when each is available):
#'
#' * `simulated` (DMAR's `"normal"`): independent normal responses of the
#'   same size, analyzed with Pearson correlations, or, for ordered items,
#'   categorized at each item's observed category proportions and analyzed
#'   with polychoric correlations;
#' * `resampled` (DMAR's `"resample"`): each item's observed responses
#'   resampled independently, which keeps every item's response
#'   distribution and removes the association, analyzed with the same
#'   correlations as the items. Garrido, Abad and Ponsoda (2013) recommend
#'   reference data modeled on the real data in this way for ordinal items.
#'
#' The run is one or two DMAR calls from one recorded seed; the page runs
#' a long one in a background R process (`sieve_pa_dmar_run()`).
#'
#' @param X A data frame of the item responses the basis was computed
#'   from.
#' @param basis The result of `sieve_retention_cor()` for the same `X`.
#' @param n_iter Number of reference datasets (at least 10): 1000 by default,
#'   `DMAR::parallel_analysis()`'s default `B`.
#' @param seed A single number, or `NULL` to draw a seed and record it.
#' @param rng Optional record from `sieve_draw_seed()`, used instead of
#'   `seed` when several analyses share one recorded seed.
#' @return A list with `ok`, `status` (`"ran"` or `"failed"`), `reason`,
#'   `error_class`, `warnings`, `messages`, `path` (`"data"` or `"matrix"`,
#'   see `sieve_pa_route()`), `route`, `call` (the DMAR calls), `basis_requested`,
#'   `basis_used`, `n_iter` (the usable reference datasets),
#'   `n_iter_requested`, `n` (DMAR's reference N), `benchmark` (whether that
#'   N is a complete-case benchmark for incomplete data), `p`, `rng`,
#'   `cores` (1), `seconds` (the time the run took), `failed` and
#'   `failed_reasons` (reference fits DMAR could not complete), `empirical`
#'   (the observed factor-analytic eigenvalues), `boundary` (items whose
#'   observed one-factor uniqueness is at its bound), `reference` (a list
#'   with `simulated` and `resampled`, see `sieve_pa_dmar_reference()`),
#'   `dmar` (DMAR's tables by reference), and `engine` (`fa.values`, the
#'   observed series the figure draws, with the references' means as
#'   `fa.sim` and `fa.simr`).
#' @author Ken Kelley
#' @noRd
sieve_parallel_analysis <- function(X, basis, n_iter = 1000L, seed = NULL, rng = NULL) {
  started <- proc.time()[["elapsed"]]
  request <- sieve_dmar_boot_request(n_iter, conf_level = 0.95, sidedness = "one")
  if (!request$ok) {
    rng <- rng %||% list(seed = seed, supplied = !is.null(seed), kind = RNGkind())
    out <- sieve_pa_not_run(basis, n_iter, rng, sieve_pa_plan(basis, n_iter))
    out$status <- "failed"
    out$n_iter_requested <- n_iter
    out$reason <- request$message
    return(out)
  }
  rng <- rng %||% sieve_draw_seed(seed)
  if (!isTRUE(basis$ok) || sieve_finite_or(basis$n, 0) < 10L) {
    out <- sieve_pa_not_run(basis, n_iter, rng, sieve_pa_plan(basis, n_iter))
    out$status <- "failed"
    out$plan <- NULL
    out$reason <- if (!isTRUE(basis$ok)) {
      basis$reason %||% "No correlation matrix is available."
    } else {
      "Parallel analysis needs ten complete cases for its reference N. The EM scree plot remains available."
    }
    out$reference <- list(simulated = sieve_pa_reference(reason = out$reason), resampled = sieve_pa_reference(reason = out$reason))
    return(out)
  }
  call <- sieve_pa_dmar_calls(X, basis, n_iter, rng$seed)
  record <- sieve_pa_dmar_run(call, rng_kind = rng$kind)
  sieve_pa_dmar_result(basis, call, record, rng, n_iter, seconds = proc.time()[["elapsed"]] - started)
}

#' The number of factors a parallel analysis reference counts
#'
#' Counts factors while the empirical eigenvalue exceeds the reference
#' eigenvalue of the same rank, stopping at the first one that does not
#' (the sequential rule, not a count of every exceedance in the series).
#' Zero is a legitimate answer: not even the first factor exceeds what
#' uncorrelated data produce.
#'
#' @param empirical Empirical eigenvalues, largest first.
#' @param reference Reference eigenvalues of the same ranks.
#' @return An integer, or `NA` when a needed comparison is undefined.
#' @author Ken Kelley
#' @noRd
sieve_pa_count <- function(empirical, reference) {
  m <- min(length(empirical), length(reference))
  if (m < 1L) {
    return(NA_integer_)
  }
  above <- empirical[seq_len(m)] > reference[seq_len(m)]
  stop_at <- which(is.na(above) | !above)[1]
  if (is.na(stop_at)) {
    return(as.integer(m))
  }
  if (is.na(above[stop_at])) NA_integer_ else as.integer(stop_at - 1L)
}

#' Read one reference series off a parallel analysis
#'
#' A display choice, not a new analysis: both statistics and both kinds of
#' reference data come from the draws the run kept.
#'
#' @param pa The result of `sieve_parallel_analysis()`.
#' @param statistic `"p95"` (95th percentile; Glorfeld, 1995) or `"mean"`
#'   (Horn, 1965).
#' @param reference `"simulated"` (the default, DMAR's `"normal"`
#'   reference, `DMAR::parallel_analysis()`'s default), `"auto"`, or
#'   `"resampled"`. `"auto"` takes the resampled reference when the items
#'   were analyzed with
#'   polychoric, tetrachoric or mixed correlations and the simulated normal
#'   reference when they were analyzed with Pearson correlations.
#' @param categorical Were the items analyzed with polychoric, tetrachoric
#'   or mixed correlations? (The Dimensionality tab passes the correlations
#'   used, not the item types.)
#' @return A list with `ok`, `suggested`, `statistic`, `family_requested`,
#'   `family_used`, `family_changed`, `reason`, `caution` (set when the
#'   reference data were analyzed with other correlations than the items),
#'   `values` (the reference series), `mc_lower` and `mc_upper` (DMAR's Monte
#'   Carlo limits of the percentile, for the 95th percentile), `n_used` (the
#'   usable reference datasets), `dmar_reference` (DMAR's name of the
#'   reference: `"normal"` or `"resample"`), `label`, and `code` (a short
#'   identifier of the series).
#' @author Ken Kelley
#' @noRd
sieve_pa_select <- function(pa, statistic = c("p95", "mean"),
                            reference = c("simulated", "auto", "resampled"),
                            categorical = FALSE) {
  statistic <- match.arg(statistic)
  reference <- match.arg(reference)
  want <- if (identical(reference, "auto")) {
    if (isTRUE(categorical)) "resampled" else "simulated"
  } else {
    reference
  }
  out <- list(
    ok = FALSE, suggested = NA_integer_, statistic = statistic,
    family_requested = want, family_used = NA_character_,
    family_changed = FALSE, reason = NULL, caution = NULL, values = NULL,
    label = NA_character_, code = NA_character_
  )
  if (is.null(pa) || !isTRUE(pa$ok)) {
    out$reason <- pa$reason %||% "Parallel analysis did not run."
    return(out)
  }
  other <- setdiff(c("simulated", "resampled"), want)
  use <- if (isTRUE(pa$reference[[want]]$available)) {
    want
  } else if (isTRUE(pa$reference[[other]]$available)) {
    other
  } else {
    NA_character_
  }
  if (is.na(use)) {
    out$reason <- pa$reference[[want]]$reason
    return(out)
  }
  ref <- pa$reference[[use]]
  out$ok <- TRUE
  out$family_used <- use
  out$family_changed <- !identical(use, want)
  if (out$family_changed) {
    out$reason <- pa$reference[[want]]$reason
  }
  if (identical(use, "simulated") && !identical(pa$basis_used, "pearson") &&
    identical(ref$correlations %||% "pearson", "pearson")) {
    # The reference should behave like the items would without common
    # factors (Garrido, Abad and Ponsoda, 2013). Polychoric, tetrachoric,
    # and mixed correlations vary more from sample to sample than Pearson
    # correlations of normal data, so this pairing sets the bar too low.
    # DMAR's normal reference for ordered items is categorized at their
    # category proportions and analyzed with polychoric correlations, so
    # the caution concerns the matrix route (mixed correlations) alone.
    out$caution <- sprintf(
      paste0(
        "The reference eigenvalues come from simulated normal data analyzed with Pearson ",
        "correlations, while the items were analyzed with %s correlations, which vary more ",
        "from sample to sample. Read against this reference, parallel analysis can suggest ",
        "too many factors, markedly so for skewed binary items (Garrido, Abad and Ponsoda, ",
        "2013, on modeling the reference data on the real data)."
      ),
      sieve_retention_basis_label(pa$basis_used)
    )
  }
  out$values <- if (identical(statistic, "p95")) ref$p95 else ref$mean
  out$mc_lower <- if (identical(statistic, "p95")) ref$mc_lower
  out$mc_upper <- if (identical(statistic, "p95")) ref$mc_upper
  out$n_used <- ref$n_used %||% pa$n_iter
  out$dmar_reference <- if (identical(use, "simulated")) "normal" else "resample"
  out$suggested <- sieve_pa_count(pa$empirical, out$values)
  out$label <- sprintf(
    "%s of the factor-analytic eigenvalues of %d datasets of %s",
    if (identical(statistic, "p95")) "95th percentile" else "mean",
    out$n_used, ref$source
  )
  out$code <- switch(paste(use, statistic),
    "simulated p95" = "quantile_95",
    "simulated mean" = "fa.sim",
    "resampled p95" = "resampled_quantile_95",
    "resampled mean" = "fa.simr"
  )
  out
}

#' Very Simple Structure From Common Factor Solutions
#'
#' Fits the maximum likelihood common factor model by
#' `DMAR::fa_exploratory()` on the correlation matrix (`S` with its `N`),
#' with direct oblimin rotation (none at one factor), for each number of
#' factors from 1 to `max_k`. For each complexity, keeps the largest absolute
#' loadings in each item row, then reproduces correlations as
#' L Phi L' (Revelle and Rocklin, 1979). The fit is one minus the ratio of
#' squared residuals to squared observed correlations, including the
#' diagonal. No component extraction or component-based criterion is
#' computed. A k whose exploratory model has negative degrees of freedom is
#' not fit, and a k DMAR refuses (an improper solution, one that did not
#' converge, or a likelihood without a maximum, read by the refusal's class:
#' `dmar_efa_no_solution` and `dmar_refusal`) has no fit; its reason is
#' kept. Any other error stops the computation. The function calls DMAR and
#' base R alone, so the recorded R carries it whole.
#' @param R Correlation matrix.
#' @param max_k Largest number of factors.
#' @param n_obs Reference sample size.
#' @param seed The seed of each rotation's random starts, or `NULL`.
#' @return Complexity 1 and 2 fit vectors (`cfit.1`, `cfit.2`), with
#'   undefined cells missing, and `reason`: for each k, `NA` when it was
#'   fit, `"not identified"`, or DMAR's reason for refusing it.
#' @author Ken Kelley
#' @noRd
sieve_vss_factors <- function(R, max_k, n_obs, seed = NULL) {
  values <- matrix(NA_real_, max_k, 2L)
  reason <- rep(NA_character_, max_k)
  total <- sum(R^2)
  p <- ncol(R)
  items <- colnames(R)
  for (k in seq_len(max_k)) {
    if ((p - k)^2 < p + k) {
      reason[k] <- "not identified"
      next
    }
    refused <- function(e) {
      list(refused = if (is.character(e$reason) && length(e$reason) == 1L) e$reason else class(e)[1L])
    }
    fit <- tryCatch(
      DMAR::fa_exploratory(
        S = R, N = n_obs, nfactors = k, estimator = "ML",
        rotation = if (k == 1L) "none" else "oblimin", conf_level = 0.95, seed = seed
      ),
      dmar_efa_no_solution = refused, dmar_refusal = refused
    )
    if (!is.null(fit$refused) && !inherits(fit, "data.frame")) {
      reason[k] <- fit$refused
      next
    }
    tab <- as.data.frame(fit)
    std <- tab[tab$metric == "standardized", , drop = FALSE]
    pattern <- std[std$op == "=~", , drop = FALSE]
    factors <- unique(as.character(pattern$lhs))
    L <- matrix(0, p, k, dimnames = list(items, factors))
    L[cbind(match(pattern$rhs, items), match(pattern$lhs, factors))] <- pattern$estimate
    Phi <- diag(k)
    off <- std[std$op == "~~" & std$lhs %in% factors & std$rhs %in% factors & std$lhs != std$rhs, , drop = FALSE]
    if (nrow(off)) {
      Phi[cbind(match(off$lhs, factors), match(off$rhs, factors))] <- off$estimate
      Phi[cbind(match(off$rhs, factors), match(off$lhs, factors))] <- off$estimate
    }
    for (complexity in seq_len(min(k, 2L))) {
      simple <- L
      for (j in seq_len(nrow(L))) {
        keep <- order(abs(L[j, ]), decreasing = TRUE)[seq_len(complexity)]
        simple[j, -keep] <- 0
      }
      values[k, complexity] <- 1 - sum((R - simple %*% Phi %*% t(simple))^2) / total
    }
  }
  list(cfit.1 = values[, 1L], cfit.2 = values[, 2L], reason = reason)
}

#' Very Simple Structure With the Recorded Random Seed
#' @param basis Result of `sieve_retention_cor()`.
#' @param max_k Largest number of factors to probe.
#' @param seed Optional seed.
#' @param rng Optional recorded RNG choice.
#' @return Factor-based fit vectors, identification flags, the k DMAR
#'   refused with its reasons (`refused`, a named character vector), and
#'   engine diagnostics.
#' @author Ken Kelley
#' @noRd
sieve_retention_vss <- function(basis, max_k = 8L, seed = NULL, rng = NULL) {
  rng <- rng %||% sieve_draw_seed(seed)
  out <- sieve_vss_shell(basis, max_k, rng)
  if (!isTRUE(basis$ok)) {
    return(out)
  }
  K <- out$max_k
  started <- proc.time()[["elapsed"]]
  # DMAR sets the seed of each rotation locally, under the generator in
  # force, and restores the caller's stream.
  run <- sieve_vss_dmar_run(sieve_vss_call(basis, K, rng), rng_kind = rng$kind)
  out$seconds <- proc.time()[["elapsed"]] - started
  sieve_vss_read(out, run)
}

#' The shape of a Very Simple Structure result before it is read
#' @param basis Result of `sieve_retention_cor()`.
#' @param max_k Largest number of factors to probe.
#' @param rng The seed record.
#' @return The list `sieve_retention_vss()` returns, not yet run
#'   (`status = "failed"` with the basis's reason when there is no matrix).
#' @author Ken Kelley
#' @noRd
sieve_vss_shell <- function(basis, max_k, rng) {
  out <- list(
    ok = FALSE, status = "failed", reason = NULL,
    warnings = character(0), messages = character(0),
    basis_requested = basis$requested, basis_used = basis$used,
    max_k = NA_integer_, k = integer(0),
    vss1 = NULL, vss2 = NULL, identified = logical(0), refused = character(0),
    max_identified = sieve_max_identified_k(basis$p), p = basis$p,
    rng = rng, engine = NULL, seconds = NA_real_
  )
  if (!isTRUE(basis$ok)) {
    out$reason <- basis$reason %||% "No correlation matrix is available."
    return(out)
  }
  out$max_k <- sieve_retention_max_k(max_k, basis$p)
  out$engine_n <- sieve_vss_n(basis)
  out$n_complete <- basis$n
  out
}

#' The numbers of factors Very Simple Structure fits
#'
#' Those from 1 to `K` whose exploratory model is identified (degrees of
#' freedom not negative); the others are not fit.
#'
#' @param K The largest number of factors probed.
#' @param p The number of items.
#' @return An integer vector.
#' @author Ken Kelley
#' @noRd
sieve_vss_ks <- function(K, p) {
  k <- seq_len(max(0L, as.integer(sieve_finite_or(K, 0L))))
  k[(p - k)^2 >= p + k]
}

#' The call of Very Simple Structure, for its runner
#' @param basis Result of `sieve_retention_cor()`.
#' @param K The largest number of factors (`sieve_retention_max_k()`).
#' @param rng The seed record.
#' @return A list with `R`, `max_k`, `n_obs` and `seed`; only plain values,
#'   so it can cross to a background R process.
#' @author Ken Kelley
#' @noRd
sieve_vss_call <- function(basis, K, rng) {
  list(R = basis$R, max_k = as.integer(K), n_obs = sieve_vss_n(basis), seed = rng$seed)
}

#' The reference N of Very Simple Structure's DMAR calls
#'
#' The basis's complete-case N (the reference size of the exploratory
#' methods, a benchmark under the saturated FIML matrix), or DMAR's FIML N
#' when fewer than three cases are complete (DMAR needs three; the point
#' estimates do not depend on it).
#'
#' @param basis Result of `sieve_retention_cor()`.
#' @return A whole number.
#' @author Ken Kelley
#' @noRd
sieve_vss_n <- function(basis) {
  n <- as.integer(basis$n)
  if (length(n) == 1L && is.finite(n) && n >= 3L) n else as.integer(basis$n_used %||% n)
}

#' Run Very Simple Structure and keep what it signaled
#'
#' The runner of the Dimensionality page's VSS: in the session, or in a
#' background R process (`sieve_efa_dmar_start(runner = "sieve_vss_dmar_run")`)
#' when it is estimated above two seconds.
#'
#' @param call A list with `R`, `max_k`, `n_obs` and `seed`.
#' @param rng_kind The three [RNGkind()] settings to run under, or `NULL`.
#' @return A record: `value` (`sieve_vss_factors()`'s result), `ok`,
#'   `error`, `warnings`, `messages`, `seconds`.
#' @author Ken Kelley
#' @noRd
sieve_vss_dmar_run <- function(call, rng_kind = NULL) {
  if (length(rng_kind) == 3L && !identical(RNGkind(), as.character(rng_kind))) {
    old <- RNGkind()
    on.exit(suppressWarnings(RNGkind(old[1L], old[2L], old[3L])), add = TRUE)
    suppressWarnings(RNGkind(rng_kind[1L], rng_kind[2L], rng_kind[3L]))
  }
  started <- proc.time()[["elapsed"]]
  run <- sieve_capture_quiet(sieve_vss_factors(call$R, call$max_k, call$n_obs, seed = call$seed))
  list(
    value = run$value, ok = isTRUE(run$ok), error = run$error,
    warnings = run$warnings %||% character(0), messages = run$messages %||% character(0),
    seconds = proc.time()[["elapsed"]] - started, fun = "vss"
  )
}

#' Read a Very Simple Structure record into the page's result
#' @param out The result shell (`sieve_retention_vss()`'s).
#' @param run A record from `sieve_vss_dmar_run()`.
#' @return The result.
#' @author Ken Kelley
#' @noRd
sieve_vss_read <- function(out, run) {
  out$warnings <- run$warnings
  out$messages <- run$messages
  if (!isTRUE(run$ok) || is.null(run$value)) {
    out$reason <- run$error %||% "Very Simple Structure stopped without a result."
    return(out)
  }
  K <- out$max_k
  out$ok <- TRUE
  out$status <- "ran"
  out$engine <- run$value
  out$k <- seq_len(K)
  out$vss1 <- as.numeric(run$value$cfit.1)
  out$vss2 <- as.numeric(run$value$cfit.2)
  out$identified <- out$k <= out$max_identified
  why <- run$value$reason
  hit <- !is.na(why) & why != "not identified"
  out$refused <- stats::setNames(why[hit], out$k[hit])
  if (!is.finite(out$seconds %||% NA_real_)) out$seconds <- run$seconds
  out
}

#' Factor model fit for each number of factors
#'
#' Fits the k factor model for k from 1 to `max_k` and keeps the test
#' statistic, RMSEA with its interval (at the level of every RMSEA interval
#' SIEVER shows, `sieve_default_rmsea_conf_level()`, 90 percent by default,
#' DMAR's and lavaan's), CFI, and TLI. The solutions are left unrotated:
#' lavaan's robust CFI, TLI and RMSEA of a rotated model fit by FIML depend
#' on the rotation (see `sieve_efa_fit_by_k()`), and an unrotated fit draws
#' no random starts. A k that could not be fit keeps the engine's reason,
#' and an index the engine did not return stays missing. A k whose model
#' has negative degrees of freedom, `((p - k)^2 - (p + k)) / 2 < 0`, has
#' more parameters than the correlations can identify; it is not fit and is
#' listed in `not_identified`.
#'
#' Every solution uses lavaan::efa on raw item data, with the same estimator
#' and missing-data treatment as the application's Model fit by k panel.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param max_k The largest number of factors to fit.
#' @param data A data frame of the included items, every row (lavaan applies
#'   the missing data treatment of `spec`), or `NULL` when unavailable.
#' @param spec The estimator specification from `sieve_lavaan_spec()`, or
#'   `NULL` when unavailable.
#' @param byk For the lavaan engine, the table of `sieve_efa_fit_by_k()` for
#'   the identified k, when it is already at hand (the Dimensionality tab
#'   builds it from the solutions its session keeps, see
#'   `sieve_efa_byk_kept()`); `NULL` fits it here.
#' @return A list with `ok`, `status`, `reason`, `engine` (`"lavaan"`),
#'   `basis_requested`, `basis_used`, `max_k`,
#'   `not_identified` (the k that were not fit), and `table`: a data frame
#'   with one row per fitted k and columns `k`, `chisq`, `df`, `pval`,
#'   `rmsea`, `rmsea_lo`, `rmsea_hi`, `cfi`, `tli`, `status`, `reason`, and
#'   `warnings`. The lavaan engine adds `estimator`, `label` (the estimator
#'   in words), `missing`, `ordered`, `family` (of CFI, TLI and RMSEA),
#'   `family_reason`, `test_family` (of the chi square), `n` (the cases
#'   lavaan used), `conf_level` (the level of the RMSEA intervals), and the
#'   columns `srmr`, `bic` and `nobs`.
#' @author Ken Kelley
#' @noRd
sieve_retention_fit <- function(basis, max_k = 8L, data = NULL, spec = NULL, byk = NULL) {
  if (is.null(data) || is.null(spec)) return(list(ok = FALSE, status = "failed", engine = "lavaan",
    reason = "Raw item data and an estimator specification are required for model fit by k.",
    basis_requested = basis$requested, basis_used = basis$used, table = NULL,
    max_k = NA_integer_, not_identified = integer(0), n = NA_integer_,
    label = "estimator unavailable", estimator = NA_character_, family = NA_character_))
  sieve_retention_fit_lavaan(basis, max_k, data, spec, byk = byk)
}

#' The missing data argument an exploratory lavaan fit is given
#'
#' Full information maximum likelihood (lavaan's `missing = "ml"`) is used
#' when some row of the items is incomplete. When none is, FIML and ML on
#' the complete cases use the same rows and give the same estimates, and
#' the complete case fit (`missing = "listwise"`) is made instead: under
#' MLR it is some forty times faster on the nine bundled tests (0.5 against
#' 20 seconds for k = 1 to 5), because lavaan's robust indices under FIML
#' fit the saturated and baseline models by EM. The robust corrections of
#' the two differ slightly (lavaan's FIML path uses the observed
#' information), so which was used is part of the result and of the
#' recorded R.
#'
#' @param missing `"fiml"`, `"listwise"`, or `NULL` (lavaan's default).
#' @param data The item data.
#' @return `"ml"`, `"listwise"`, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_missing_arg <- function(missing, data) {
  switch(missing %||% "default",
    fiml = ,
    ml = if (anyNA(data)) "ml" else "listwise",
    listwise = "listwise",
    NULL
  )
}

#' The missing data treatment of an exploratory lavaan fit, in words
#'
#' @param lavaan_missing The `missing` argument lavaan was given.
#' @param missing The treatment the estimator asks for (`"fiml"` or
#'   `"listwise"`).
#' @return A phrase.
#' @author Ken Kelley
#' @noRd
sieve_efa_missing_words <- function(lavaan_missing, missing = NULL) {
  if (identical(lavaan_missing, "ml")) {
    "every case with at least one response, under FIML"
  } else if (identical(missing, "fiml")) {
    "every case, all complete, so FIML reduces to the complete cases"
  } else {
    "the complete cases"
  }
}

#' The correlations a lavaan estimator specification works from
#'
#' @param spec The result of `sieve_lavaan_spec()`.
#' @param types The confirmed types of the included items.
#' @return A basis identifier (see `sieve_retention_basis_labels()`).
#' @author Ken Kelley
#' @noRd
sieve_spec_basis <- function(spec, types) {
  n_ord <- length(spec$ordered)
  if (!n_ord) {
    return("pearson")
  }
  if (n_ord < length(types)) {
    return("mixed")
  }
  if (all(types == "binary")) "tetrachoric" else "polychoric"
}

#' The lavaan engine of the fit sequence
#'
#' @inheritParams sieve_retention_fit
#' @return See `sieve_retention_fit()`.
#' @author Ken Kelley
#' @noRd
sieve_retention_fit_lavaan <- function(basis, max_k, data, spec, byk = NULL) {
  data <- as.data.frame(data)
  p <- ncol(data)
  types <- if (!is.null(basis$types) && length(basis$types) == p) {
    as.character(basis$types)
  } else {
    ifelse(names(data) %in% (spec$ordered %||% character(0)), "ordered", "continuous")
  }
  used <- sieve_spec_basis(spec, types)
  out <- list(
    ok = FALSE, status = "failed", reason = NULL, engine = "lavaan",
    estimator = spec$estimator, label = spec$label, missing = spec$missing,
    ordered = spec$ordered, basis_requested = used, basis_used = used,
    max_k = NA_integer_, not_identified = integer(0), table = NULL,
    family = NA_character_, family_reason = NA_character_,
    test_family = NA_character_, n = NA_integer_
  )

  K <- sieve_retention_max_k(max_k, p)
  out$max_k <- K
  ks <- seq_len(K)
  identified <- sieve_efa_df(p, ks) >= 0
  out$not_identified <- ks[!identified]
  if (!any(identified)) {
    out$reason <- "No factor model is identified for this few items."
    return(out)
  }
  # Unrotated: the fit indices are read from the unrotated solution (see
  # sieve_efa_fit_by_k()), and no rotation draws random starts.
  if (is.null(byk)) {
    # An item lavaan cannot fit is refused by name before any model is fit;
    # the refusal is this record's reason.
    byk <- tryCatch(
      sieve_efa_fit_by_k(data, spec$estimator, spec$ordered,
        rotation = "none",
        ks = ks[identified], missing = spec$missing
      ),
      sieve_unusable_items = function(e) e
    )
    if (inherits(byk, "sieve_unusable_items")) {
      out$reason <- conditionMessage(byk)
      return(out)
    }
  }
  solved <- byk$status %in% c("ok", "inadmissible")
  warn <- vapply(attr(byk, "warnings"), function(w) {
    if (length(w)) paste(unique(gsub("\\s+", " ", w)), collapse = " | ") else NA_character_
  }, character(1))
  out$table <- data.frame(
    k = byk$k,
    chisq = byk$chisq, df = byk$df, pval = byk$pvalue,
    rmsea = byk$RMSEA, rmsea_lo = byk$RMSEA.lo, rmsea_hi = byk$RMSEA.hi,
    cfi = byk$CFI, tli = byk$TLI, srmr = byk$SRMR, bic = byk$BIC, nobs = byk$nobs,
    status = ifelse(solved, "ran", "failed"),
    reason = ifelse(solved,
      ifelse(byk$status == "inadmissible", gsub("\\s+", " ", byk$message), NA_character_),
      gsub("\\s+", " ", byk$message)
    ),
    warnings = warn,
    stringsAsFactors = FALSE
  )
  out$family <- attr(byk, "family")
  out$conf_level <- attr(byk, "conf_level") %||% NA_real_
  out$lavaan_missing <- attr(byk, "lavaan_missing")
  out$family_reason <- attr(byk, "family_reason")
  out$test_family <- attr(byk, "test_family")
  out$n <- suppressWarnings(as.integer(max(byk$nobs[solved], na.rm = TRUE)))
  if (!is.finite(out$n)) out$n <- NA_integer_
  out$ok <- any(out$table$status == "ran")
  out$status <- if (out$ok) "ran" else "failed"
  if (!out$ok) {
    out$reason <- unique(out$table$reason[!is.na(out$table$reason)])[1]
  }
  out
}

#' A fit sequence that has not been run
#'
#' The record the Dimensionality tab holds while a lavaan fit sequence that
#' would take longer than the threshold waits for its button.
#'
#' @param spec The result of `sieve_lavaan_spec()`.
#' @param plan A list with `ks` (the numbers of factors), `p`, `label` (the
#'   estimated wait in words), and `workers` (the background workers the
#'   wait assumes).
#' @param basis_used The correlations the estimator works from.
#' @return A list in the shape of `sieve_retention_fit()`'s, with `status =
#'   "not run"`.
#' @author Ken Kelley
#' @noRd
sieve_fit_not_run <- function(spec, plan, basis_used = "pearson") {
  ks <- plan$ks
  list(
    ok = FALSE, status = "not run", engine = "lavaan",
    reason = sprintf(
      paste0(
        "The model fit by k has not run yet. It would fit %d exploratory model%s (k = %s) to %d items ",
        "with lavaan::efa under %s; the estimated wait is %s%s. Press Run the model fit by k to start it."
      ),
      length(ks), if (length(ks) == 1L) "" else "s",
      if (length(ks) > 1L) sprintf("%d to %d", min(ks), max(ks)) else as.character(ks),
      plan$p, spec$label, plan$label, sieve_efa_workers_words(plan$workers %||% 1L, length(ks))
    ),
    estimator = spec$estimator, label = spec$label, missing = spec$missing, ordered = spec$ordered,
    basis_requested = basis_used, basis_used = basis_used,
    max_k = max(c(ks, plan$not_identified)), not_identified = plan$not_identified %||% integer(0),
    table = NULL, family = NA_character_, family_reason = NA_character_,
    test_family = NA_character_, n = NA_integer_, plan = plan
  )
}

#' The R that replays the lavaan fit sequence
#'
#' @param fit The result of `sieve_retention_fit()` with the lavaan engine.
#' @param data_name The name of the included item data in the script.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_fit_lavaan_code <- function(fit, data_name = "active_items") {
  if (!isTRUE(fit$ok)) {
    return(sieve_comment_lines("Not run:", fit$reason))
  }
  ran <- fit$table$k
  ks <- if (length(ran) > 1L && identical(as.integer(ran), seq.int(ran[1], ran[length(ran)]))) {
    sprintf("%d:%d", ran[1], ran[length(ran)])
  } else {
    sprintf("c(%s)", paste(ran, collapse = ", "))
  }
  family <- fit$family
  suffix <- if (is.na(family %||% NA_character_)) "" else switch(family,
    robust = ".robust",
    scaled = ".scaled",
    ""
  )
  test_suffix <- if (identical(fit$test_family, "scaled")) ".scaled" else ""
  measures <- c(
    paste0(c("chisq", "df", "pvalue"), test_suffix),
    paste0(c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi", "tli"), suffix),
    sieve_srmr_measure(), "bic"
  )
  ordered <- if (length(fit$ordered)) {
    sprintf(", ordered = c(%s)", paste(sprintf("\"%s\"", fit$ordered), collapse = ", "))
  } else {
    ""
  }
  missing <- switch(fit$lavaan_missing %||% "default",
    ml = ", missing = \"ml\"",
    listwise = ", missing = \"listwise\"",
    ""
  )
  c(
    if (length(fit$not_identified)) {
      sieve_comment_lines("Not fit:", sprintf(
        "k = %s (negative degrees of freedom: more parameters than the correlations identify).",
        paste(fit$not_identified, collapse = ", ")
      ))
    },
    sprintf(
      "# Model fit by k: lavaan::efa at each k under %s, every row of the included",
      fit$label %||% fit$estimator
    ),
    sprintf("# items (%s). Unrotated: the", sieve_efa_missing_words(fit$lavaan_missing, fit$missing)),
    sprintf(
      "# robust CFI, TLI and RMSEA of a rotated efa object fit by FIML depend on the rotation. CFI, TLI and RMSEA in the %s",
      if (is.na(family %||% NA_character_)) "(no common)" else family
    ),
    sprintf("# family for every k; the chi square is the %s statistic. The RMSEA interval is at %s,",
      fit$test_family %||% "standard",
      sieve_ci_percent(sieve_valid_conf_level(fit$conf_level, sieve_default_rmsea_conf_level()))),
    "# the level of every RMSEA interval SIEVER shows (90% by default, DMAR's and lavaan's).",
    sprintf("fit_by_k <- do.call(rbind, lapply(%s, function(k) {", ks),
    sprintf(
      "  f <- lavaan::efa(data = %s, nfactors = k, rotation = \"none\", estimator = \"%s\"%s%s)",
      data_name, fit$estimator, ordered, missing
    ),
    "  if (inherits(f, \"efaList\")) f <- f[[1]]",
    sprintf("  m <- lavaan::fitMeasures(f, c(%s), fm.args = list(rmsea.ci.level = %s))",
      paste(sprintf("\"%s\"", measures), collapse = ", "),
      sieve_num_lit(sieve_valid_conf_level(fit$conf_level, sieve_default_rmsea_conf_level()))),
    if (is.na(family %||% NA_character_)) {
      "  m[c(4:8)] <- NA_real_  # no common family: no fit-index substitution"
    } else {
      sieve_zero_df_mask_code()
    },
    "  data.frame(k = k, chisq = m[[1]], df = m[[2]], p = m[[3]], rmsea = m[[4]], rmsea_lo = m[[5]],",
    "             rmsea_hi = m[[6]], cfi = m[[7]], tli = m[[8]], srmr = m[[9]], bic = m[[10]])",
    "}))",
    "# The indices are read as factors are added, the RMSEA with its interval; no",
    "# cutoff is applied to them and no number of factors is read from them.",
    "fit_by_k"
  )
}

#' The evidence on the number of factors, criterion by criterion
#'
#' One row per factor-analytic criterion: parallel analysis and the two
#' Very Simple Structure complexities, which belong to one family. A
#' criterion that did not run, or that ran and produced no count, has a
#' missing `suggested` and a `reason`: it is unavailable, never counted as
#' agreeing.
#'
#' The model fit by k (`sieve_retention_fit()`) is not a criterion and
#' gives no count: the indices are read as factors are added, the RMSEA
#' with its interval, and no value of RMSEA, CFI or TLI is a property of a good
#' exploratory solution, so no number of factors is read off the sequence
#' by a cutoff.
#'
#' Very Simple Structure is computed from factor loadings, so its count is
#' read only over the `k` whose exploratory model is identified (see
#' `sieve_vss_count()`). Parallel analysis counts eigenvalues and
#' can give a number larger than the largest
#' identified `k` in a small item pool; that number is reported as the
#' criterion gives it, with a note that a solution that large cannot be fit
#' as an exploratory model.
#'
#' @param basis,pa,vss Results of `sieve_retention_cor()`,
#'   `sieve_parallel_analysis()`, and `sieve_retention_vss()`.
#' @param statistic,reference,categorical Passed to `sieve_pa_select()`.
#' @return A data frame with columns `id`, `criterion`, `family`,
#'   `suggested`, `at_least` (for Very Simple Structure whose
#'   minimum or maximum lies at the largest number probed: the lower bound,
#'   with `suggested` empty, since a larger probe may move it), `basis_requested`, `basis_used`, `basis_changed`,
#'   `status` (`"ran"`, `"no count"`, or `"failed"`), `reason`,
#'   `assumptions`, `produced_by`, `setting` (for parallel analysis, the
#'   reference the count was read against), `stochastic`, `seed`, and
#'   `max_identified` (the largest number of factors whose exploratory model
#'   is identified for these items, the same on every row).
#' @author Ken Kelley
#' @noRd
sieve_retention_criteria <- function(basis, pa, vss,
                                     statistic = "p95", reference = "simulated",
                                     categorical = FALSE) {
  sel <- sieve_pa_select(pa, statistic, reference, categorical)
  used <- sieve_retention_basis_label(basis$used)
  max_id <- sieve_max_identified_k(basis$p)
  row <- function(id, criterion, family, suggested, status, reason, assumptions,
                  produced_by, stochastic, seed, setting = NA_character_, at_least = NA_integer_) {
    data.frame(
      id = id, criterion = criterion, family = family,
      suggested = as.integer(suggested), at_least = as.integer(at_least),
      basis_requested = basis$requested, basis_used = basis$used,
      basis_changed = isTRUE(basis$changed),
      status = status,
      reason = if (is.null(reason) || !length(reason)) NA_character_ else paste(reason,
        collapse = " "
      ),
      assumptions = assumptions, produced_by = produced_by, setting = setting,
      stochastic = stochastic, seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
      max_identified = max_id,
      stringsAsFactors = FALSE
    )
  }
  with_basis <- function(reason) {
    # A change of correlations is part of every affected criterion's record:
    # the engine's reason, then what the response categories look like.
    c(if (isTRUE(basis$changed)) c(sub("[.]?$", ".", basis$reason), basis$facts), reason)
  }
  # A count that an exploratory factor model cannot carry is still what the
  # criterion gives, so it is reported, with what it cannot be used for.
  beyond <- function(k, counted = "factors") {
    if (length(k) != 1L || is.na(k) || k <= max_id) {
      return(NULL)
    }
    sprintf(
      paste0(
        "The count of %d %s is more than the %d factor%s whose exploratory model is ",
        "identified with %d items (a model with more has negative degrees of freedom ",
        "and no unique solution), so a solution that large cannot be fit and compared ",
        "as an exploratory model."
      ),
      k, counted, max_id, if (max_id == 1L) "" else "s", basis$p
    )
  }

  pa_status <- if (identical(pa$status, "not run")) {
    "not run"
  } else if (!isTRUE(pa$ok) || !sel$ok) {
    "failed"
  } else if (is.na(sel$suggested)) {
    "no count"
  } else {
    "ran"
  }
  pa_row <- row(
    "pa", "Parallel analysis (Horn, 1965)", "Parallel analysis",
    sel$suggested, pa_status,
    with_basis(c(
      if (!isTRUE(pa$ok)) pa$reason,
      if (isTRUE(pa$ok) && sieve_finite_or(pa$failed, 0) > 0) {
        sprintf(
          "%d reference fit%s could not be completed (%s) and %s left out, as DMAR counts them; the reference describes the null distribution of the fits that returned eigenvalues.",
          as.integer(pa$failed), if (identical(as.integer(pa$failed), 1L)) "" else "s",
          paste(pa$failed_reasons, collapse = "; "), if (identical(as.integer(pa$failed), 1L)) "is" else "are"
        )
      },
      if (isTRUE(pa$ok) && length(pa$boundary)) {
        sprintf(
          "The observed one-factor reduction reaches the lower uniqueness bound (0.005) for %s; read the reduced eigenvalues with that boundary in mind.",
          sieve_name_some(pa$boundary)
        )
      },
      if (isTRUE(pa$ok)) sel$reason,
      if (isTRUE(pa$ok)) sel$caution,
      if (isTRUE(pa$ok) && sel$ok && is.na(sel$suggested)) "An eigenvalue comparison was undefined.",
      if (isTRUE(pa$ok) && sel$ok) beyond(sel$suggested)
    )),
    paste0(
      "Counts factors while the empirical factor-analytic eigenvalue exceeds the ",
      "same-rank eigenvalue of uncorrelated reference data, stopping at the first ",
      "that does not. Assumes the reference data behave like the observed items ",
      "would without common factors; it can miss weak or highly correlated factors ",
      "(Lim and Jahng, 2019)."
    ),
    if (isTRUE(pa$ok) && sel$ok) {
      sprintf(
        paste0(
          "DMAR::parallel_analysis(): the fixed one-factor maximum likelihood reduction (stats::factanal) ",
          "of the %s and of every reference dataset; reference: %s."
        ),
        sieve_pa_matrix_words(pa), sel$label
      )
    } else {
      "DMAR::parallel_analysis(): the fixed one-factor maximum likelihood reduction (stats::factanal) of the correlations and of every reference dataset."
    },
    TRUE, pa$rng$seed,
    setting = if (isTRUE(pa$ok) && sel$ok) {
      sprintf(
        "%s of %d %s datasets",
        if (identical(sel$statistic, "p95")) "95th percentile" else "mean", sel$n_used %||% pa$n_iter,
        if (identical(sel$family_used, "simulated")) "simulated normal" else "resampled"
      )
    } else {
      NA_character_
    }
  )

  vss_ok <- isTRUE(vss$ok)
  # VSS waits with the other requested analyses until Update.
  vss_waits <- identical(vss$status, "not run")
  probe_more <- function(k, top) {
    sprintf(
      paste0(
        "The optimum is at the largest number probed, k = %d, so the count is at least %d and ",
        "may be larger: raise the largest number of factors probed (now %d; at most %d with %d ",
        "items) to see where it lies."
      ),
      k, k, vss$max_k, top, vss$p
    )
  }
  vss_status <- function(k) if (vss_waits) "not run" else if (!vss_ok) "failed" else if (is.na(k)) "no count" else "ran"
  # Very Simple Structure is read from factor loadings, so only over the k
  # whose exploratory model is identified and at which the complexity is
  # defined, and only when at least two such k can be compared.
  # A k DMAR refused (an improper solution, one that did not converge, or
  # a likelihood without a maximum) has no fit to compare; it is named.
  refused_text <- if (vss_ok && length(vss$refused)) {
    sprintf(
      "DMAR refused the solution at %s, so %s no fit to compare.",
      paste(sprintf("k = %s (%s)", names(vss$refused), vss$refused), collapse = " and "),
      if (length(vss$refused) == 1L) "it has" else "they have"
    )
  }
  with_refused <- function(why) {
    out <- paste(c(why, refused_text), collapse = " ")
    if (nzchar(out)) out else NULL
  }
  vss_read <- function(v, complexity) {
    if (!vss_ok) {
      return(list(k = NA_integer_, reason = vss$reason))
    }
    cnt <- sieve_vss_count(v, vss$identified, complexity)
    if (is.na(cnt$k)) {
      return(list(k = NA_integer_, reason = with_refused(sprintf(
        paste0(
          "No count is read: Very Simple Structure compares numbers of factors, and %s ",
          "can be compared here (complexity %d needs at least %d factor%s, k was probed up ",
          "to %d, and the exploratory model is identified up to k = %d with %d items)."
        ),
        if (length(cnt$candidates)) sprintf("only k = %d", cnt$candidates) else "no k",
        complexity, complexity, if (complexity == 1L) "" else "s",
        vss$max_k, vss$max_identified, vss$p
      ))))
    }
    if (cnt$k >= vss$max_k && vss$max_k < vss$max_identified) {
      return(list(k = NA_integer_, at_least = cnt$k, reason = with_refused(probe_more(cnt$k, vss$max_identified))))
    }
    list(k = cnt$k, reason = with_refused(if (!identical(cnt$unrestricted, cnt$k)) {
      sprintf(
        paste0(
          "The fit is larger still at k = %d, but an exploratory model with that many ",
          "factors is not identified with %d items (negative degrees of freedom, so its ",
          "loadings are not unique); the count is read over k = %d to %d."
        ),
        cnt$unrestricted, vss$p, min(cnt$candidates), max(cnt$candidates)
      )
    }))
  }
  vss_text <- function(c) {
    sprintf(
      paste0(
        "Fit of a simplified pattern that keeps each item's %s; assumes simple ",
        "structure of that complexity, so it favors fewer factors when items load ",
        "on several (Revelle and Rocklin, 1979)."
      ),
      if (c == 1L) "single largest loading" else "two largest loadings"
    )
  }
  vss_by <- sprintf(
    "DMAR::fa_exploratory ML factor loadings and factor correlations, direct oblimin rotation, %s correlation matrix.", used
  )
  vss1 <- vss_read(vss$vss1, 1L)
  vss2 <- vss_read(vss$vss2, 2L)
  vss1_row <- row(
    "vss1", "Very Simple Structure, complexity 1 (Revelle and Rocklin, 1979)", "VSS",
    vss1$k, vss_status(vss1$k), with_basis(vss1$reason),
    vss_text(1L), vss_by, TRUE, vss$rng$seed, at_least = vss1$at_least %||% NA_integer_
  )
  vss2_row <- row(
    "vss2", "Very Simple Structure, complexity 2 (Revelle and Rocklin, 1979)", "VSS",
    vss2$k, vss_status(vss2$k), with_basis(vss2$reason),
    vss_text(2L), vss_by, TRUE, vss$rng$seed, at_least = vss2$at_least %||% NA_integer_
  )

  out <- rbind(pa_row, vss1_row, vss2_row)
  rownames(out) <- NULL
  out
}

#' Agreement and disagreement among the retention criteria
#'
#' Disagreement is a finding, so the summary leads with the range of counts
#' and which criteria suggest which. The tally that follows is a heuristic:
#' the criteria are computed from the same correlation matrix and are not
#' independent, so the most often suggested count has no error rate and is
#' not a consensus estimate. A family is one kind of evidence and counts
#' toward a number only when every one of its criteria that produced a count
#' suggests that number: Very Simple Structure, shown at two complexities,
#' therefore counts once when the two agree, and when they differ it is
#' reported as divided and counts toward neither. Each family thus enters
#' the tally at most once, and the family counts over the suggested numbers
#' never add up to more than the number of families. A criterion without a
#' count is unavailable and supports nothing. The plus or minus one
#' neighborhood (Lim and Jahng, 2019) is a range of solutions worth fitting
#' and comparing, not a confidence interval; it is limited to the numbers of
#' factors whose exploratory model is identified, since a solution that
#' cannot be fit cannot be examined.
#'
#' @param criteria The data frame from `sieve_retention_criteria()`.
#' @param theory_k The theorized number of factors, or `NULL`.
#' @return A list with `available` and `unavailable` (criterion ids), `range`
#'   (smallest and largest count, or `NA`), `agree`, `by_k` (a data frame:
#'   `k`, `criteria` (every criterion suggesting `k`), `families` (the
#'   families whose criteria all suggest `k`), `n_families`), `n_families`
#'   (families with at least one count), `divided` (a data frame: `family`,
#'   `criteria`, `counts`, one row per family whose criteria suggest
#'   different numbers), `modal` (the count or tied counts supported by the
#'   most families; empty when no family supports a single count),
#'   `neighborhood` (lower and upper end of the heuristic range),
#'   `max_identified` (the largest identified number of factors, or `NA`
#'   when the criteria do not carry it), `capped` (was the neighborhood
#'   limited by it), `beyond` (ids of the criteria whose count is larger
#'   than it), and `theory` (`k`, `in_suggested`, `in_neighborhood`), or
#'   `NULL` when no theorized count was given.
#' @author Ken Kelley
#' @noRd
sieve_retention_summary <- function(criteria, theory_k = NULL) {
  have <- is.finite(criteria$suggested)
  avail <- criteria[have, , drop = FALSE]
  max_id <- if ("max_identified" %in% names(criteria) && nrow(criteria)) {
    as.integer(criteria$max_identified[1])
  } else {
    NA_integer_
  }
  out <- list(
    available = criteria$id[have], unavailable = criteria$id[!have],
    range = c(NA_integer_, NA_integer_), agree = NA,
    by_k = data.frame(
      k = integer(0), criteria = character(0), families = character(0),
      n_families = integer(0), stringsAsFactors = FALSE
    ),
    n_families = 0L,
    divided = data.frame(
      family = character(0), criteria = character(0), counts = character(0),
      stringsAsFactors = FALSE
    ),
    modal = integer(0),
    neighborhood = c(NA_integer_, NA_integer_),
    max_identified = max_id, capped = FALSE, beyond = character(0), theory = NULL
  )
  if (nrow(avail)) {
    ks <- sort(unique(avail$suggested))
    out$range <- as.integer(range(avail$suggested))
    out$agree <- length(ks) == 1L
    # What each family suggests, over its criteria that produced a count.
    families <- unique(avail$family)
    family_ks <- lapply(families, function(f) sort(unique(avail$suggested[avail$family == f])))
    names(family_ks) <- families
    united <- vapply(family_ks, length, integer(1)) == 1L
    out$by_k <- do.call(rbind, lapply(ks, function(k) {
      rows <- avail[avail$suggested == k, , drop = FALSE]
      fams <- families[united & vapply(family_ks, function(v) v[1] == k, logical(1))]
      data.frame(
        k = as.integer(k), criteria = paste(rows$id, collapse = ", "),
        families = paste(fams, collapse = ", "), n_families = length(fams),
        stringsAsFactors = FALSE
      )
    }))
    out$n_families <- length(families)
    if (any(!united)) {
      out$divided <- do.call(rbind, lapply(families[!united], function(f) {
        rows <- avail[avail$family == f, , drop = FALSE]
        data.frame(
          family = f, criteria = paste(rows$id, collapse = ", "),
          counts = paste(rows$suggested, collapse = ", "), stringsAsFactors = FALSE
        )
      }))
    }
    top <- max(out$by_k$n_families)
    out$modal <- if (top > 0L) out$by_k$k[out$by_k$n_families == top] else integer(0)
    # With no family behind a single count, every suggested count anchors the range.
    anchor <- if (length(out$modal)) out$modal else as.integer(ks)
    lo <- min(anchor) - 1L
    out$neighborhood <- c(if (min(anchor) >= 1L) max(1L, lo) else 0L, max(anchor) + 1L)
    if (!is.na(max_id)) {
      out$beyond <- avail$id[avail$suggested > max_id]
      if (out$neighborhood[2] > max_id) {
        out$capped <- TRUE
        out$neighborhood <- c(min(out$neighborhood[1], max_id), max_id)
      }
    }
  }
  if (!is.null(theory_k) && length(theory_k) == 1L && is.finite(theory_k)) {
    tk <- as.integer(theory_k)
    out$theory <- list(
      k = tk,
      in_suggested = nrow(avail) > 0L && tk %in% avail$suggested,
      in_neighborhood = nrow(avail) > 0L && tk >= out$neighborhood[1] && tk <= out$neighborhood[2]
    )
  }
  out
}

#' Compute the Evidence on the Number of Factors in One Call
#'
#' The plain R entry point: the correlation matrix, parallel analysis
#' and Very Simple Structure, and the fit sequence, from one recorded seed.
#' The Dimensionality tab calls the same parts separately so that changing
#' one setting recomputes only what depends on it.
#'
#' @param X A data frame of numeric item responses. Continuous analyses use
#'   saturated FIML correlations and FIML model fits. Ordered analyses, and
#'   polychoric correlations requested for continuous items (`cor = "poly"`),
#'   use complete cases. Parallel analysis uses complete-case reference N.
#' @param types Character vector of confirmed item types, one per column.
#' @param cor `"auto"`, `"pearson"`, or `"poly"`; see
#'   `sieve_retention_request()`.
#' @param n_iter Number of random datasets for parallel analysis: 1000 by
#'   default, `DMAR::parallel_analysis()`'s default `B`.
#' @param max_k The largest number of factors to probe.
#' @param seed A single number, or `NULL` (the default) to draw a seed from
#'   the session's stream and record it.
#' @return A list of class `sieve_retention` with `basis`, `pa`, `vss`,
#'   `fit`, `rng`, `n`, `n_rows`, `p`, and `categorical`.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
#' # Two hundred meets DMAR's minimum for the 95th percentile reference.
#' evidence <- sieve_retention_evidence(items, rep("continuous", 6), n_iter = 200L,
#'   max_k = 2L, seed = 1)
#' evidence$pa$empirical
#' evidence$vss[c("vss1", "vss2")]
#' evidence$fit$table[, c("k", "chisq", "df", "rmsea", "rmsea_lo", "rmsea_hi", "cfi")]
#' @export
sieve_retention_evidence <- function(X, types = NULL, cor = "auto", n_iter = 1000L,
                                     max_k = 8L, seed = NULL) {
  X <- as.data.frame(X)
  n_rows <- nrow(X)
  X[] <- lapply(X, sieve_as_number)
  types <- if (is.null(types)) rep("continuous", ncol(X)) else as.character(types)
  requested <- sieve_retention_request(cor, types)
  categorical <- any(types %in% c("ordered", "binary"))
  raw_items <- X
  if (categorical || !identical(requested, "pearson")) X <- X[stats::complete.cases(X), , drop = FALSE]
  rng <- sieve_draw_seed(seed)
  # Saturated FIML is a Pearson route: the other bases use the complete
  # rows set aside above.
  basis <- sieve_retention_cor(X, types, requested,
    missing = if (categorical || !identical(requested, "pearson")) "listwise" else "fiml")
  out <- list(
    basis = basis,
    pa = sieve_parallel_analysis(X, basis, n_iter = n_iter, rng = rng),
    vss = sieve_retention_vss(basis, max_k = max_k, rng = rng),
    fit = sieve_retention_fit(basis, max_k = max_k, data = if (categorical) X else raw_items,
      spec = sieve_lavaan_spec(data.frame(item = names(X), type = types, status = "active"))),
    rng = rng, n = basis$n, n_rows = n_rows, p = ncol(X),
    categorical = categorical
  )
  class(out) <- "sieve_retention"
  out
}

#' The R that computes the retention correlation matrix
#'
#' Written from the result, so it records the correlations that were used,
#' and, when they are not the ones requested, what was requested and why it
#' changed.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param data_name The name of the included item data in the script.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_retention_cor_code <- function(basis, data_name = "active_items") {
  if (identical(basis$missing, "fiml") && isTRUE(basis$ok)) return(c(
    "# Saturated FIML covariance estimated by EM; no values are imputed.",
    sprintf("em_data <- %s", sieve_reliability_data_code(list(aux = basis$aux), data_name)),
    sieve_numeric_code("em_data"),
    sprintf("em_moments <- DMAR::cov_fiml(em_data%s)",
      if (length(basis$aux)) paste0(", aux = ", sieve_r_literal(basis$aux)) else ""),
    "R_items <- stats::cov2cor(em_moments$S)",
    sprintf("complete_rows <- stats::complete.cases(em_data[, %s, drop = FALSE])", sieve_r_literal(basis$items)),
    "n_complete <- sum(complete_rows)",
    "# The complete-case N is the reference size of the exploratory methods: a benchmark, not an effective N or a guaranteed conservative choice."
  ))
  rows <- sprintf("%s[complete_rows, , drop = FALSE]", data_name)
  chr <- function(x) sprintf("c(%s)", paste(sprintf("\"%s\"", x), collapse = ", "))
  head <- c(
    "# Complete cases only: every retention criterion below uses the same rows.",
    sprintf("complete_rows <- stats::complete.cases(%s)", data_name),
    "n_complete <- sum(complete_rows)"
  )
  if (!isTRUE(basis$ok)) {
    return(c(head, sieve_comment_lines("Not run:", basis$reason)))
  }
  note <- if (isTRUE(basis$changed)) {
    c(
      sieve_comment_lines("Requested:", sprintf("%s correlations.", sieve_retention_basis_label(basis$requested))),
      sieve_comment_lines("Used:", sprintf(
        "%s correlations instead. This changes the analysis.", sieve_retention_basis_label(basis$used)
      )),
      sieve_comment_lines("Reason:", basis$reason),
      if (!is.null(basis$facts)) sieve_comment_lines("Data:", basis$facts)
    )
  } else {
    sprintf(
      "# %s correlations, as requested.",
      paste0(
        toupper(substring(sieve_retention_basis_label(basis$used), 1L, 1L)),
        substring(sieve_retention_basis_label(basis$used), 2L)
      )
    )
  }
  # What the engine reported while the matrix was computed belongs to the
  # record of the analysis as much as the call does.
  said <- c(basis$warnings, basis$messages)
  reported <- if (length(said)) sieve_comment_lines("Reported:", said)
  # psych computes these correlations pair by pair inside
  # parallel::mcmapply(); on one worker its notices are shown when the
  # script is run, as they were kept when the application ran it.
  one_worker <- function(lines) {
    c(
      "# Keep psych's internal pairwise work serial so all notices are retained.",
      "# Independent resampling tasks use SIEVER's outer socket pool.",
      "old_options <- options(mc.cores = 1L)",
      lines,
      "options(old_options)"
    )
  }
  call <- switch(basis$used,
    pearson = sprintf("R_items <- stats::cor(%s)", rows),
    polychoric = one_worker(
      sprintf("R_items <- psych::polychoric(%s, correct = 0.5, progress = FALSE)$rho", rows)
    ),
    tetrachoric = one_worker(
      sprintf("R_items <- psych::tetrachoric(%s, correct = 0.5)$rho", rows)
    ),
    mixed = {
      cont <- basis$items[!basis$types %in% c("ordered", "binary")]
      poly <- basis$items[basis$types == "ordered"]
      dich <- basis$items[basis$types == "binary"]
      c(
        "# Mixed correlations from the confirmed measurement levels: c = continuous,",
        "# p = ordered (polytomous), d = binary (dichotomous).",
        one_worker(c(
          sprintf("R_items <- psych::mixedCor(%s,", rows),
          sprintf("  c = %s,", if (length(cont)) chr(cont) else "NULL"),
          sprintf("  p = %s,", if (length(poly)) chr(poly) else "NULL"),
          sprintf("  d = %s,", if (length(dich)) chr(dich) else "NULL"),
          "  correct = 0.5)$rho"
        )),
        sprintf("R_items <- R_items[names(%s), names(%s)]", data_name, data_name)
      )
    }
  )
  c(head, note, reported, call)
}

#' The R of one DMAR parallel analysis call
#'
#' @param call One call of `sieve_pa_dmar_calls()`.
#' @param result The name the script gives its table.
#' @param data_name,matrix_name,n_name The names of the item data, the
#'   correlation matrix and its reference N in the script.
#' @param rng_kind The [RNGkind()] settings the run used; the call is
#'   wrapped in [withr::with_seed()] under them, since DMAR's `seed` seeds
#'   the generator in force.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_pa_call_code <- function(call, result, data_name = "active_items", matrix_name = "R_items",
                               n_name = "n_complete", rng_kind = NULL) {
  a <- call$args
  parts <- c(
    if (!is.null(a$S)) c(sprintf("S = %s", matrix_name), sprintf("N = %s", n_name)) else sprintf("data = %s", data_name),
    if (!is.null(a$missing)) sprintf("missing = \"%s\"", a$missing),
    if (isTRUE(a$ordered)) "ordered = TRUE",
    sprintf("reference = \"%s\"", a$reference),
    sprintf("B = %d", as.integer(a$B)),
    sprintf("percentile = %s", format(a$percentile)),
    sprintf("conf_level = %s", format(a$conf_level)),
    sprintf("seed = %d", as.integer(a$seed))
  )
  body <- c(
    "DMAR::parallel_analysis(",
    paste0("  ", parts, c(rep(",", length(parts) - 1L), "")),
    ")"
  )
  if (length(rng_kind) == 3L) {
    return(c(
      sprintf("%s <- withr::with_seed(%d, .rng_kind = %s, .rng_normal_kind = %s, .rng_sample_kind = %s,",
        result, as.integer(a$seed), encodeString(rng_kind[1L], quote = "\""),
        encodeString(rng_kind[2L], quote = "\""), encodeString(rng_kind[3L], quote = "\"")),
      paste0("  ", body),
      ")"
    ))
  }
  c(paste0(result, " <- ", body[1L]), body[-1L])
}

#' The R that replays a parallel analysis and its selected reference
#'
#' The DMAR calls the run made, every argument written out with the
#' recorded seed (one call per reference), then the reference the page
#' reads, the count, and the figure. The script gives the tables the
#' application holds.
#'
#' @param pa The result of `sieve_parallel_analysis()`.
#' @param sel The result of `sieve_pa_select()` for it.
#' @param data_name The name of the included item data in the script.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_pa_code <- function(pa, sel, data_name = "active_items") {
  if (!isTRUE(pa$ok)) {
    return(sieve_comment_lines("Not run:", pa$reason))
  }
  route <- pa$route
  calls <- pa$call$calls
  head <- c(
    "# Parallel analysis by DMAR::parallel_analysis() (Horn, 1965): the eigenvalues of the",
    "# correlation matrix reduced by a fixed one-factor maximum likelihood solution",
    "# (stats::factanal), against the same reduction of every reference dataset.",
    sieve_comment_lines("Correlations:", sprintf("the %s.", sieve_pa_matrix_words(pa))),
    if (identical(route$form, "matrix")) sieve_comment_lines("Why the matrix:", paste0(route$why, ".")),
    sprintf("# One call per reference data, each from seed %d%s; DMAR sets it locally.", as.integer(pa$rng$seed),
      if (isTRUE(pa$rng$supplied)) "" else " (no seed was supplied; SIEVER drew this one so the run can be replayed)")
  )
  per <- unlist(lapply(names(calls), function(family) {
    c(
      sieve_pa_call_code(calls[[family]], paste0("pa_", family), data_name = data_name, rng_kind = pa$rng$kind),
      sprintf("pa_%s  # the table with the Monte Carlo limits of the reference percentile, as DMAR prints it", family),
      sprintf("attr(pa_%s, \"counts\")  # usable, failed and boundary reference fits", family)
    )
  }), use.names = FALSE)
  if (!isTRUE(sel$ok)) {
    return(c(head, per, sieve_comment_lines("Reference:", sel$reason)))
  }
  obj <- paste0("pa_", sel$dmar_reference)
  c(
    head, per,
    sieve_comment_lines("Reference read:", paste0(sel$label, ".")),
    sprintf("observed <- %s$estimate", obj),
    if (identical(sel$statistic, "p95")) {
      sprintf("reference <- %s$reference_quantile  # the 95th percentile (Glorfeld, 1995)", obj)
    } else {
      sprintf("reference <- %s$reference_mean  # the mean (Horn, 1965)", obj)
    },
    "# Count factors while the observed eigenvalue exceeds the reference, stopping at",
    "# the first one that does not.",
    "n_factors <- sum(cumprod(observed > reference))",
    "# The figure: the empirical series (circles, solid) and the reference",
    "# (triangles, dashed); the solid gray line at 1 is for orientation only.",
    "plot(observed, type = \"b\", pch = 16, xlab = \"Factor\",",
    "     ylab = \"Factor-analytic eigenvalue (reduced correlation matrix)\")",
    "lines(reference, type = \"b\", pch = 17, lty = 2)",
    "abline(h = 1, lty = 1, col = \"gray50\")",
    "legend(\"topright\", c(\"empirical\", \"reference\"), pch = c(16, 17), lty = c(1, 2))"
  )
}

#' The R That Replays Factor-Based Very Simple Structure
#'
#' @param vss The result of `sieve_retention_vss()`.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_vss_code <- function(vss) {
  if (!isTRUE(vss$ok)) {
    return(sieve_comment_lines("Not run:", vss$reason))
  }
  kind <- vss$rng$kind
  n_text <- if (!is.null(vss$engine_n) && !identical(as.integer(vss$engine_n), as.integer(vss$n_complete))) {
    as.character(vss$engine_n)
  } else {
    "n_complete"
  }
  c(
    "# Very Simple Structure from DMAR::fa_exploratory() solutions (maximum likelihood on",
    "# the correlation matrix, direct oblimin from 30 random starts, none at one factor):",
    "# each call sets the recorded seed locally under the generator the app used and",
    "# restores the session's stream. A k DMAR refuses (an improper solution, one that did",
    "# not converge, or a likelihood without a maximum) has no fit; its reason is kept.",
    sprintf("# Rotation starts: seed = %d%s.", vss$rng$seed,
      if (isTRUE(vss$rng$supplied)) "" else " (no seed was supplied; SIEVER drew this one so the run can be replayed)"),
    paste0("sieve_vss_factors <- ", paste(deparse(sieve_vss_factors), collapse = "\n")),
    if (length(kind) == 3L) {
      sprintf(
        "vss_out <- withr::with_seed(%d, .rng_kind = %s, .rng_normal_kind = %s, .rng_sample_kind = %s,\n  sieve_vss_factors(R_items, %d, %s, seed = %d))",
        vss$rng$seed, encodeString(kind[1L], quote = "\""), encodeString(kind[2L], quote = "\""),
        encodeString(kind[3L], quote = "\""), vss$max_k, n_text, vss$rng$seed
      )
    } else {
      sprintf("vss_out <- sieve_vss_factors(R_items, %d, %s, seed = %d)", vss$max_k, n_text, vss$rng$seed)
    },
    "vss_out$reason  # NA where fit; \"not identified\", or DMAR's reason for a refused k",
    "# Very Simple Structure is computed from factor loadings, so it is read only",
    "# over the k whose exploratory model is identified (degrees of freedom",
    "# ((p - k)^2 - (p + k)) / 2 not negative) and at which the complexity is",
    "# defined (k at least the complexity), and only when two or more such k can",
    "# be compared. Values at unidentified k are not read as evidence.",
    "k_probed <- seq_along(vss_out$cfit.1)",
    "n_items <- ncol(R_items)",
    "identified <- ((n_items - k_probed)^2 - (n_items + k_probed)) / 2 >= 0",
    "vss_count <- function(fit, complexity) {",
    "  k <- k_probed[identified & k_probed >= complexity & is.finite(fit)]",
    "  if (length(k) < 2L) NA_integer_ else k[which.max(fit[k])]",
    "}",
    "vss1_k <- vss_count(vss_out$cfit.1, 1L)",
    "vss2_k <- vss_count(vss_out$cfit.2, 2L)",
    "possible_k <- seq_len(n_items - 1L)",
    "max_identified <- max(0L, possible_k[((n_items - possible_k)^2 - (n_items + possible_k)) / 2 >= 0])",
    "at_boundary <- function(k) !is.na(k) && k == length(k_probed) && k < max_identified",
    "vss1_at_least <- if (at_boundary(vss1_k)) vss1_k else NA_integer_",
    "vss2_at_least <- if (at_boundary(vss2_k)) vss2_k else NA_integer_",
    "if (!is.na(vss1_at_least)) vss1_k <- NA_integer_",
    "if (!is.na(vss2_at_least)) vss2_k <- NA_integer_"
  )
}

#' The R that replays the fit sequence
#'
#' @param fit The result of `sieve_retention_fit()`.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_fit_code <- function(fit) {
  if (identical(fit$engine, "lavaan")) {
    return(sieve_fit_lavaan_code(fit))
  }
  # The model fit by k is lavaan::efa()'s (sieve_retention_fit()); a record
  # that names another engine (psych::fa()) or no engine is not replayed.
  sieve_comment_lines("Not run:", fit$reason %||% "the model fit by k is computed by lavaan::efa(); this record names no engine.")
}

#' Short names of the retention criteria for sentences
#'
#' @param id Criterion identifiers.
#' @return A character vector of names.
#' @author Ken Kelley
#' @noRd
sieve_retention_names <- function(id) {
  unname(c(
    pa = "parallel analysis", map = "MAP", vss1 = "VSS complexity 1",
    vss2 = "VSS complexity 2"
  )[id])
}

#' The retention evidence in plain sentences
#'
#' Turns the criterion rows and their summary into the sentences the tab
#' shows: the range and who disagrees with whom first, then what was not
#' available, then the tally and the neighborhood, each named as the
#' heuristic it is. Kept as a plain function so the wording that carries
#' the interpretation is tested and not buried in a renderer.
#'
#' The heading says what kind of statement follows. "Agreement" is used
#' only when at least two criterion families produced a count and every
#' count is the same: one criterion, or one family, cannot agree with
#' anything, and no count at all is not disagreement.
#'
#' @param criteria The data frame from `sieve_retention_criteria()`.
#' @param summary The list from `sieve_retention_summary()` for it.
#' @return A named list of character strings: `heading`, `tone` (`"info"`,
#'   `"warning"`, or `"secondary"`, for the alert that carries the range),
#'   `range`, `unavailable`, `tally`, `neighborhood`, `identified` (set when
#'   a criterion suggests more factors than an exploratory model
#'   identifies), and `theory` (each `NULL` when it does not apply).
#' @author Ken Kelley
#' @noRd
sieve_retention_statements <- function(criteria, summary) {
  plural <- function(k) sprintf("%d factor%s", k, if (k == 1L) "" else "s")
  join <- function(x) {
    if (length(x) <= 1L) return(x)
    paste(paste(x[-length(x)], collapse = ", "), x[length(x)], sep = " and ")
  }
  family_name <- function(f) {
    unname(c(
      "Parallel analysis" = "parallel analysis", MAP = "MAP",
      VSS = "Very Simple Structure"
    )[f])
  }
  sentence_start <- function(x) paste0(toupper(substring(x, 1L, 1L)), substring(x, 2L))
  n_avail <- length(summary$available)
  one_family <- n_avail > 1L && summary$n_families == 1L
  out <- list(
    heading = NULL, tone = NULL, range = NULL, unavailable = NULL, tally = NULL,
    neighborhood = NULL, identified = NULL, theory = NULL
  )
  if (!n_avail) {
    out$heading <- "No count."
    out$tone <- "warning"
    out$range <- "No criterion produced a count for the current item pool."
  } else if (n_avail == 1L) {
    out$heading <- "One criterion only."
    out$tone <- "secondary"
    out$range <- sprintf(
      paste0(
        "Only %s produced a count: it suggests %s. One criterion is not agreement; ",
        "the others are unavailable for the reasons given below."
      ),
      sieve_retention_names(summary$available), plural(summary$range[1])
    )
  } else if (isTRUE(summary$agree) && one_family) {
    out$heading <- "One criterion family only."
    out$tone <- "secondary"
    out$range <- sprintf(
      paste0(
        "The %d criteria that produced a count belong to one family (%s) and suggest %s. ",
        "One kind of evidence is not agreement among criteria."
      ),
      n_avail, family_name(criteria$family[match(summary$available[1], criteria$id)]),
      plural(summary$range[1])
    )
  } else if (isTRUE(summary$agree)) {
    out$heading <- "Agreement."
    out$tone <- "info"
    out$range <- sprintf(
      paste0(
        "All %d criteria that produced a count suggest %s. Agreement among criteria ",
        "computed from one sample and one correlation matrix is weaker evidence than the ",
        "same agreement among independent replications."
      ),
      n_avail, plural(summary$range[1])
    )
  } else {
    out$heading <- "Disagreement is a finding."
    out$tone <- "warning"
    who <- vapply(seq_len(nrow(summary$by_k)), function(i) {
      ids <- strsplit(summary$by_k$criteria[i], ", ", fixed = TRUE)[[1]]
      sprintf("%s (%s)", plural(summary$by_k$k[i]), join(sieve_retention_names(ids)))
    }, character(1))
    out$range <- sprintf(
      "The criteria disagree: the suggested number of factors ranges from %d to %d. They suggest %s.",
      summary$range[1], summary$range[2], paste(who, collapse = "; ")
    )
  }
  if (length(summary$unavailable)) {
    rows <- criteria[match(summary$unavailable, criteria$id), , drop = FALSE]
    waiting <- rows$status == "not run"
    parts <- c(
      if (any(waiting)) {
        # The reason of a criterion that waits is a paragraph of its own (the
        # wait and the button that starts the run), so it follows the names.
        sprintf(
          "Not run yet, and not in the tally until it is: %s. %s",
          join(sieve_retention_names(rows$id[waiting])),
          # The two VSS rows share one run and one reason.
          paste(unique(sub("([^.])$", "\\1.", rows$reason[waiting])), collapse = " ")
        )
      },
      if (any(!waiting)) {
        sprintf(
          "Not available, and not counted as agreeing with anything: %s.",
          paste(
            sprintf(
              "%s (%s)", sieve_retention_names(rows$id[!waiting]),
              ifelse(is.na(rows$reason[!waiting]), rows$status[!waiting],
                sub("[.]$", "", rows$reason[!waiting])
              )
            ),
            collapse = "; "
          )
        )
      }
    )
    out$unavailable <- paste(parts, collapse = " ")
  }
  if (n_avail) {
    top <- summary$by_k[summary$by_k$k %in% summary$modal, , drop = FALSE]
    lead <- if (!nrow(top)) {
      "no number is suggested by a whole criterion family."
    } else if (nrow(top) == 1L) {
      fams <- strsplit(top$families, ", ", fixed = TRUE)[[1]]
      sprintf(
        "%s is suggested by %d of the %d criterion families that produced a count (%s).",
        plural(top$k), top$n_families, summary$n_families, join(family_name(fams))
      )
    } else {
      sprintf(
        "no single count leads; %s are each suggested by %d of the %d criterion families that produced a count.",
        join(as.character(top$k)), top$n_families[1], summary$n_families
      )
    }
    split <- if (!is.null(summary$divided) && nrow(summary$divided)) {
      paste(vapply(seq_len(nrow(summary$divided)), function(i) {
        ids <- strsplit(summary$divided$criteria[i], ", ", fixed = TRUE)[[1]]
        ks <- strsplit(summary$divided$counts[i], ", ", fixed = TRUE)[[1]]
        sprintf(
          " %s is divided (%s) and counts toward none of them.",
          sentence_start(family_name(summary$divided$family[i])),
          join(sprintf("%s suggests %s", sieve_retention_names(ids), ks))
        )
      }, character(1)), collapse = "")
    } else {
      ""
    }
    out$tally <- paste0(
      "Heuristic tally, not a calibrated consensus: ", lead, split,
      " A family counts toward a number only when every one of its criteria that ",
      "produced a count suggests it, so Very Simple Structure, shown at two ",
      "complexities, counts once at most. The criteria share one sample and one ",
      "correlation matrix, so they are not independent and the tally has no known ",
      "error rate."
    )
    lo <- summary$neighborhood[1]
    hi <- summary$neighborhood[2]
    out$neighborhood <- paste0(
      sprintf(
        "Heuristic range of solutions worth examining: %s (%s plus or minus one; Lim and Jahng, 2019).",
        if (lo == hi) plural(lo) else sprintf("%d to %d factors", lo, hi),
        if (!nrow(top)) {
          "the suggested counts"
        } else if (nrow(top) == 1L) {
          "the most often suggested count"
        } else {
          "the most often suggested counts"
        }
      ),
      if (isTRUE(summary$capped)) {
        sprintf(
          paste0(
            " The range stops at %d, the largest number of factors whose ",
            "exploratory model is identified with these items."
          ),
          summary$max_identified
        )
      } else {
        ""
      },
      " It is a range of solutions to fit and compare on interpretability, not a ",
      "confidence interval for the number of factors."
    )
  }
  if (length(summary$beyond)) {
    rows <- criteria[match(summary$beyond, criteria$id), , drop = FALSE]
    out$identified <- sprintf(
      paste0(
        "More than an exploratory model can carry: %s. With these items an exploratory ",
        "factor model is identified for at most %s (a model with more has negative ",
        "degrees of freedom and no unique solution). Each count is reported as its ",
        "criterion gives it; a solution that large cannot be fit and compared here."
      ),
      join(sprintf("%s suggests %d", sieve_retention_names(rows$id), rows$suggested)),
      plural(summary$max_identified)
    )
  }
  if (!is.null(summary$theory)) {
    tk <- summary$theory$k
    rows <- criteria[is.finite(criteria$suggested) & criteria$suggested == tk, , drop = FALSE]
    out$theory <- paste0(
      sprintf("The theorized structure has %s. ", plural(tk)),
      if (!n_avail) {
        ""
      } else if (nrow(rows)) {
        sprintf(
          "That count is suggested by %s%s. ", join(sieve_retention_names(rows$id)),
          if (isTRUE(summary$theory$in_neighborhood)) " and lies within the heuristic range" else ""
        )
      } else {
        sprintf(
          "No criterion suggests that count, and it lies %s the heuristic range. ",
          if (isTRUE(summary$theory$in_neighborhood)) "within" else "outside"
        )
      },
      "The theorized count is a reference for reading the evidence, not an override of it."
    )
  }
  out
}

#' What was looked at, for a factor retention decision record
#'
#' @param criteria The data frame from `sieve_retention_criteria()`.
#' @param theory_k The theorized number of factors, or `NULL`.
#' @return A character vector, one entry per criterion.
#' @author Ken Kelley
#' @noRd
sieve_retention_evidence_lines <- function(criteria, theory_k = NULL) {
  bound <- if ("at_least" %in% names(criteria)) criteria$at_least else rep(NA_integer_, nrow(criteria))
  shown <- ifelse(is.finite(criteria$suggested), as.character(criteria$suggested),
    ifelse(is.finite(bound), sprintf("at least %d (%s)", bound, criteria$reason),
      sprintf("not available (%s)", ifelse(is.na(criteria$reason), criteria$status, criteria$reason))
    )
  )
  c(
    sprintf(
      "%s: %s [%s correlations%s%s%s]",
      sieve_retention_names(criteria$id), shown,
      sieve_retention_basis_label(criteria$basis_used),
      ifelse(criteria$basis_changed,
        sprintf(", %s requested", sieve_retention_basis_label(criteria$basis_requested)), ""
      ),
      ifelse(is.na(criteria$setting), "", sprintf("; %s", criteria$setting)),
      ifelse(is.na(criteria$seed), "", sprintf("; seed %d", criteria$seed))
    ),
    if (!is.null(theory_k) && length(theory_k) == 1L && is.finite(theory_k)) {
      sprintf("theorized number of factors: %d", as.integer(theory_k))
    }
  )
}

# ---- The scree plot --------------------------------------------------------

#' The largest number of factors a scree plot's reduced series extracts
#'
#' The Ledermann bound: the largest `k` for which a `k`-factor model of
#' `p` variables has nonnegative degrees of freedom,
#' `((p - k)^2 - (p + k)) / 2 >= 0`, a necessary (not a sufficient)
#' condition for the maximum likelihood communalities to be identified; a
#' solution near the bound is often improper, which
#' `sieve_pa_empirical()` reports. At least 1.
#'
#' @param p The number of variables.
#' @return An integer.
#' @author Ken Kelley
#' @noRd
sieve_scree_max_factors <- function(p) {
  p <- as.integer(p)
  if (length(p) != 1L || is.na(p) || p < 2L) {
    return(1L)
  }
  k <- seq_len(p)
  ok <- ((p - k)^2 - (p + k)) / 2 >= 0
  max(1L, if (any(ok)) max(k[ok]) else 1L)
}

#' The number of factors a scree plot's reduced series extracts, as given
#'
#' A whole number from 1 to `sieve_scree_max_factors(p)`; anything else
#' (empty, not a number, below 1) is 1, and a number above the bound is the
#' bound.
#'
#' @param x The control's value.
#' @param p The number of variables.
#' @return An integer.
#' @author Ken Kelley
#' @noRd
sieve_scree_nfactors <- function(x, p) {
  k <- suppressWarnings(as.integer(x))
  if (length(k) != 1L || is.na(k) || k < 1L) {
    return(1L)
  }
  min(k, sieve_scree_max_factors(p))
}

#' A number of factors in words, as an adjective
#'
#' @param k A whole number.
#' @return `"one-factor"` through `"nine-factor"`, then `"10-factor"` and
#'   so on.
#' @author Ken Kelley
#' @noRd
sieve_scree_factor_words <- function(k) {
  k <- as.integer(k)
  words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  sprintf("%s-factor", if (!is.na(k) && k >= 1L && k <= 9L) words[k] else as.character(k))
}

#' The vertical limits of an eigenvalue plot's window
#'
#' A narrowed window fits the vertical axis to the eigenvalues it shows
#' (the line at 1 does not stretch it); the full plot keeps ggplot2's
#' own limits, which include the line at 1.
#'
#' @param eigenvalues The eigenvalues drawn (every series, in the window).
#' @param window The result of `sieve_rank_window()`.
#' @param n The number of eigenvalues.
#' @return `c(low, high)`, or `NULL` for the full plot.
#' @author Ken Kelley
#' @noRd
sieve_rank_window_ylim <- function(eigenvalues, window, n) {
  ev <- eigenvalues[is.finite(eigenvalues)]
  if (!length(ev) || identical(as.integer(window), c(1L, as.integer(n)))) {
    return(NULL)
  }
  range(ev)
}

#' Whether the line at 1 lies in a window's vertical range
#'
#' @param ylim The result of `sieve_rank_window_ylim()`.
#' @return `TRUE` when the line is drawn in the window (always for the
#'   full plot).
#' @author Ken Kelley
#' @noRd
sieve_rank_window_has_one <- function(ylim) {
  if (is.null(ylim)) {
    return(TRUE)
  }
  pad <- 0.05 * max(diff(ylim), 1e-8)
  1 >= ylim[1L] - pad && 1 <= ylim[2L] + pad
}

#' The theorized and recorded numbers of factors as vertical lines
#'
#' Each line carries its label at the top of the eigenvalues drawn, to the
#' right of the line, or to its left when the line is the window's last
#' rank, so that the label is never cut off at the edge of the panel.
#'
#' @param gg A ggplot.
#' @param window The result of `sieve_rank_window()`.
#' @param eigenvalues The eigenvalues drawn.
#' @param theory_k,decision_k The lines to draw (already within the
#'   window), or `NULL`.
#' @return The ggplot with the lines added.
#' @author Ken Kelley
#' @noRd
sieve_rank_window_vlines <- function(gg, window, eigenvalues, theory_k = NULL, decision_k = NULL) {
  ev <- eigenvalues[is.finite(eigenvalues)]
  top <- max(ev)
  span <- max(diff(range(ev)), 1e-8)
  given <- function(k) !is.null(k) && length(k) == 1L && is.finite(k)
  side <- function(k) if (k >= window[2L]) 1.1 else -0.1
  if (given(theory_k)) {
    gg <- gg +
      ggplot2::geom_vline(xintercept = theory_k, linetype = "dashed", color = "#5b6770", linewidth = 0.6) +
      ggplot2::annotate("text",
        x = theory_k, y = top, label = "theory k", hjust = side(theory_k), vjust = 1,
        color = "#5b6770", size = 3.6
      )
  }
  if (given(decision_k)) {
    gg <- gg +
      ggplot2::geom_vline(xintercept = decision_k, linetype = "dotdash", color = "#8a3b12", linewidth = 0.6) +
      ggplot2::annotate("text",
        x = decision_k, y = top - 0.15 * span, label = "decision k", hjust = side(decision_k), vjust = 1,
        color = "#8a3b12", size = 3.6
      )
  }
  gg
}

#' The eigenvalues a scree plot draws
#'
#' Two series from the shared correlation basis: the eigenvalues of the
#' correlation matrix as it stands (the unreduced matrix, whose eigenvalues
#' the Kaiser criterion concerns), and the factor-analytic eigenvalues of
#' the matrix reduced by the communalities of a maximum likelihood
#' solution with `nfactors` factors (see `sieve_pa_empirical()`; with one
#' factor, the series parallel analysis compares). The number of unreduced
#' eigenvalues above one is counted as the Kaiser criterion gives it, and
#' reported as a reference and not as a rule.
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param nfactors The number of factors whose communalities reduce the
#'   matrix, held to `sieve_scree_nfactors()`.
#' @return A list with `ok`, `reason` (why not, or `NULL`), `table` (a data
#'   frame with `k`, `unreduced`, and `reduced`, or `NULL`), `kaiser` (the
#'   count of unreduced eigenvalues above one), `nfactors` (the number of
#'   factors extracted for the reduced series), `improper` (the items whose
#'   uniqueness in that solution is at its lower bound, a Heywood case;
#'   empty when it is proper), `basis_used`, `n`, `p`, `warnings`, and
#'   `messages`.
#' @author Ken Kelley
#' @noRd
sieve_scree_values <- function(basis, nfactors = 1L) {
  nfactors <- sieve_scree_nfactors(nfactors, basis$p)
  out <- list(
    ok = FALSE, reason = NULL, table = NULL, kaiser = NA_integer_, nfactors = nfactors,
    improper = character(0), basis_used = basis$used, n = basis$n, p = basis$p,
    warnings = character(0), messages = character(0)
  )
  if (!isTRUE(basis$ok)) {
    out$reason <- basis$reason %||% "No correlation matrix could be computed."
    return(out)
  }
  unreduced <- eigen(basis$R, symmetric = TRUE, only.values = TRUE)$values
  reduced <- {
    sieve_pa_empirical(basis, nfactors)
  }
  red <- rep(NA_real_, length(unreduced))
  if (!is.null(reduced$values)) {
    red[seq_len(min(length(red), length(reduced$values)))] <-
      reduced$values[seq_len(min(length(red), length(reduced$values)))]
  }
  out$ok <- TRUE
  out$table <- data.frame(k = seq_along(unreduced), unreduced = unreduced, reduced = red)
  out$kaiser <- sum(unreduced > 1)
  out$improper <- reduced$improper %||% character(0)
  out$warnings <- reduced$warnings
  out$messages <- reduced$messages
  out$reason <- reduced$error
  out
}

#' The axis breaks of an eigenvalue rank axis
#'
#' Every rank when 25 or fewer are drawn; beyond that the first rank drawn
#' and every fifth, so the labels stay legible on a large item pool.
#'
#' @param n The last rank drawn.
#' @param from The first rank drawn.
#' @return An integer vector of breaks.
#' @author Ken Kelley
#' @noRd
sieve_rank_breaks <- function(n, from = 1L) {
  n <- as.integer(n)
  from <- as.integer(from)
  if (n - from + 1L <= 25L) {
    return(seq.int(from, n))
  }
  # A multiple of five within two ranks of the first rank drawn is left
  # out, so its label does not run into that rank's (24 and 25 read "2425").
  fifths <- seq.int(5L * as.integer(ceiling(from / 5)), n, by = 5L)
  c(from, fifths[fifths - from >= 3L])
}

#' The ranks an eigenvalue plot shows
#'
#' The display window of a scree plot: all `n` ranks unless a narrower
#' range is asked for. A request is two whole numbers within 1 to `n`; a
#' range of one rank (both ends of the slider on one rank) is widened to
#' the nearest two ranks, so that a line is drawn and the plot stays where
#' the slider is; anything else is all ranks. The window only changes what
#' is drawn; nothing is recomputed.
#'
#' @param n The number of eigenvalues.
#' @param shown The requested range, `c(from, to)`, or `NULL`.
#' @return An integer vector `c(from, to)`.
#' @author Ken Kelley
#' @noRd
sieve_rank_window <- function(n, shown = NULL) {
  n <- as.integer(n)
  all_ranks <- c(1L, max(1L, n))
  r <- suppressWarnings(as.integer(round(as.numeric(shown))))
  if (length(r) != 2L || anyNA(r) || n < 2L) {
    return(all_ranks)
  }
  r <- sort(pmin(pmax(r, 1L), n))
  if (r[2L] == r[1L]) {
    r <- if (r[1L] < n) c(r[1L], r[1L] + 1L) else c(n - 1L, n)
  }
  r
}

#' The R that draws an eigenvalue plot's display window
#'
#' What a saved figure shows beyond the recorded R, which draws every
#' rank: the window's ranks, with the vertical axis fitted to them.
#'
#' @param window The result of `sieve_rank_window()`.
#' @param n The number of eigenvalues.
#' @param first,second The names of the two series in the recorded R.
#' @param xlab,ylab The axis labels.
#' @return R code lines, or `NULL` when every rank is shown.
#' @author Ken Kelley
#' @noRd
sieve_rank_window_code <- function(window, n, first, second, xlab, ylab) {
  if (n < 2L || identical(as.integer(window), c(1L, as.integer(n)))) {
    return(NULL)
  }
  c(
    sprintf("# Ranks %d to %d of %d were shown, with the vertical axis fitted to them:", window[1L], window[2L], n),
    sprintf("shown <- %d:%d", window[1L], window[2L]),
    sprintf("plot(shown, %s[shown], type = \"b\", pch = 16, xlab = \"%s\", ylab = \"%s\",", first, xlab, ylab),
    sprintf("     ylim = range(c(%s[shown], %s[shown]), na.rm = TRUE))", first, second),
    sprintf("lines(shown, %s[shown], type = \"b\", pch = 17, lty = 2)", second),
    "abline(h = 1, lty = 1, col = \"gray50\")"
  )
}

#' A sentence that says which ranks a plot shows, when not all
#'
#' @param window The result of `sieve_rank_window()`.
#' @param n The number of eigenvalues.
#' @return A sentence, or `NULL` when every rank is shown.
#' @author Ken Kelley
#' @noRd
sieve_rank_window_text <- function(window, n) {
  if (identical(as.integer(window), c(1L, as.integer(n)))) {
    return(NULL)
  }
  sprintf(
    "Ranks %d to %d of %d are shown, with the vertical axis fitted to them; the full plot is ranks 1 to %d.",
    window[1L], window[2L], n, n
  )
}

#' The vertical reference lines of an eigenvalue plot that fall in its window
#'
#' The theorized number of factors and the recorded decision are drawn as
#' vertical lines when they lie within the ranks shown; one that lies
#' outside is named in a sentence instead, so a narrowed window never
#' widens itself to reach a line and never drops one silently.
#'
#' @param window The result of `sieve_rank_window()`.
#' @param theory_k,decision_k The theorized number of factors and the
#'   number the researcher recorded, or `NULL`.
#' @return A list with `theory_k` and `decision_k` (each the value, or
#'   `NULL` when absent or outside the window) and `outside` (a sentence, or
#'   `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_rank_window_lines <- function(window, theory_k = NULL, decision_k = NULL) {
  given <- function(k) !is.null(k) && length(k) == 1L && is.finite(k)
  inside <- function(k) given(k) && k >= window[1L] && k <= window[2L]
  out <- character(0)
  if (given(theory_k) && !inside(theory_k)) {
    out <- c(out, sprintf("the theorized number of factors (%d)", as.integer(theory_k)))
  }
  if (given(decision_k) && !inside(decision_k)) {
    out <- c(out, sprintf("the recorded decision (%d)", as.integer(decision_k)))
  }
  list(
    theory_k = if (inside(theory_k)) theory_k else NULL,
    decision_k = if (inside(decision_k)) decision_k else NULL,
    outside = if (length(out)) {
      sprintf(
        "Not drawn because outside the ranks shown: %s.",
        paste(out, collapse = " and ")
      )
    }
  )
}

#' The scree plot
#'
#' Both series of `sieve_scree_values()` on one figure, told apart by line
#' type and point shape as well as color, with a legend; a horizontal line
#' at 1 labeled as the Kaiser criterion, a reference and not a rule; and
#' the theorized number and the recorded decision as vertical lines when
#' there are any. `shown` narrows the ranks drawn (see
#' `sieve_rank_window()`) so that the leading eigenvalues can be read on a
#' large item pool; the series are not recomputed.
#'
#' @param values The result of `sieve_scree_values()`.
#' @param palette The palette name (see `sieve_palette_colors()`).
#' @param theory_k,decision_k The theorized number of factors and the
#'   number the researcher recorded, or `NULL`.
#' @param shown The ranks to draw, `c(from, to)`, or `NULL` for all.
#' @return A ggplot object, or `NULL` when there is nothing to draw.
#' @author Ken Kelley
#' @noRd
sieve_scree_plot <- function(values, palette = "nd", theory_k = NULL, decision_k = NULL,
                             shown = NULL) {
  if (!isTRUE(values$ok) || is.null(values$table)) {
    return(NULL)
  }
  tab <- values$table
  window <- sieve_rank_window(nrow(tab), shown)
  lines_in <- sieve_rank_window_lines(window, theory_k, decision_k)
  theory_k <- lines_in$theory_k
  decision_k <- lines_in$decision_k
  tab <- tab[tab$k >= window[1L] & tab$k <= window[2L], , drop = FALSE]
  cols <- sieve_palette_colors(palette, 5L)
  basis_label <- sieve_retention_basis_label(values$basis_used)
  unreduced_label <- sprintf("Correlation matrix (unreduced; %s correlations)", basis_label)
  reduced_label <- sprintf(
    "Reduced correlation matrix (factor analytic: %s ML communalities)",
    sieve_scree_factor_words(values$nfactors %||% 1L)
  )
  long <- rbind(
    data.frame(k = tab$k, eigenvalue = tab$unreduced, series = unreduced_label, stringsAsFactors = FALSE),
    data.frame(k = tab$k, eigenvalue = tab$reduced, series = reduced_label, stringsAsFactors = FALSE)
  )
  long <- long[is.finite(long$eigenvalue), , drop = FALSE]
  long$series <- factor(long$series, levels = c(unreduced_label, reduced_label))
  ylim <- sieve_rank_window_ylim(long$eigenvalue, window, nrow(values$table))
  has_one <- sieve_rank_window_has_one(ylim)
  # c() drops the sentences that do not apply; paste() alone would leave an
  # empty string, and a double space, in their place.
  sub <- paste(c(
    paste0(
      "The solid gray line at 1 is the Kaiser criterion (eigenvalue 1): a reference, not a rule. It ",
      "concerns the eigenvalues of the unreduced correlation matrix, tends to retain too many ",
      "factors, and has no inferential basis."
    ),
    if (identical(as.integer(values$nfactors %||% 1L), 1L)) {
      "The factor-analytic series is what parallel analysis compares with its reference."
    } else {
      "Parallel analysis compares the one-factor reduction, not this one."
    },
    sieve_scree_improper_text(values),
    sieve_rank_window_text(window, nrow(values$table)),
    if (!has_one) "The line at 1 lies outside the range of the eigenvalues shown.",
    lines_in$outside
  ), collapse = " ")
  # The label of the line at 1 ends short of the last rank when a vertical
  # line stands there, so the two do not overlap.
  end_line <- any(c(theory_k, decision_k) >= window[2L])
  gg <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .data$k, y = .data$eigenvalue, color = .data$series,
      linetype = .data$series, shape = .data$series, group = .data$series
    )
  ) +
    # A solid reference line, gray and thinner than the series (0.8), so it
    # reads as a reference and not as a third series.
    ggplot2::geom_hline(yintercept = 1, linetype = "solid", color = "#8a929a", linewidth = 0.45) +
    # Labeled on the full plot only: a narrowed window can run its series
    # through the label, and its subtitle names the line.
    ggplot2::annotate("text",
      x = window[2L] - if (end_line) 0.15 else 0, y = 1,
      label = if (is.null(ylim)) "Kaiser criterion (eigenvalue 1)" else "",
      hjust = 1, vjust = -0.4, color = "#5b6770", size = 3.4
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_color_manual(
      values = stats::setNames(cols[1:2], c(unreduced_label, reduced_label)),
      name = NULL, drop = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(c("solid", "longdash"), c(unreduced_label, reduced_label)),
      name = NULL, drop = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = stats::setNames(c(16, 17), c(unreduced_label, reduced_label)),
      name = NULL, drop = FALSE
    )
  gg <- sieve_rank_window_vlines(gg, window, long$eigenvalue, theory_k, decision_k)
  if (!is.null(ylim)) {
    gg <- gg + ggplot2::coord_cartesian(ylim = ylim)
  }
  gg +
    ggplot2::scale_x_continuous(breaks = sieve_rank_breaks(window[2L], window[1L])) +
    ggplot2::labs(
      x = "Eigenvalue rank (factor)", y = "Eigenvalue",
      subtitle = paste(strwrap(sub, width = 120), collapse = "\n")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.key.width = grid::unit(2.2, "lines"),
      plot.subtitle = ggplot2::element_text(color = "#5b6770", size = 10)
    )
}

#' The R that reproduces the scree plot's two series
#'
#' Reads `R_items` from the basis step of the recorded script.
#'
#' @param values The result of `sieve_scree_values()`.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_scree_code <- function(values) {
  if (!isTRUE(values$ok)) {
    return(sieve_comment_lines("Not run:", values$reason %||% "no correlation matrix"))
  }
  k <- as.integer(values$nfactors %||% 1L)
  c(
    "# Scree plot: the eigenvalues of the correlation matrix as it stands (the",
    "# Kaiser criterion, eigenvalue 1, concerns these; a reference, not a rule)",
    "# and the factor-analytic eigenvalues of the matrix reduced by the",
    sprintf("# communalities of a %s maximum likelihood solution (the diagonal of", sieve_scree_factor_words(k)),
    "# ones replaced by the communalities, then the eigenvalues taken).",
    "# The solution is stats::factanal's, the fixed reduction DMAR::parallel_analysis() uses.",
    "scree_unreduced <- eigen(R_items, symmetric = TRUE, only.values = TRUE)$values",
    sprintf(
      "scree_fit <- stats::factanal(covmat = R_items, factors = %d, rotation = \"none\")",
      k
    ),
    "scree_reduced_matrix <- R_items",
    "diag(scree_reduced_matrix) <- 1 - scree_fit$uniquenesses",
    "scree_reduced <- eigen(scree_reduced_matrix, symmetric = TRUE, only.values = TRUE)$values",
    "kaiser_count <- sum(scree_unreduced > 1)  # a reference, not a rule",
    "plot(scree_unreduced, type = \"b\", pch = 16, xlab = \"Eigenvalue rank\", ylab = \"Eigenvalue\")",
    "lines(scree_reduced, type = \"b\", pch = 17, lty = 2)",
    "abline(h = 1, lty = 1, col = \"gray50\")  # Kaiser criterion (eigenvalue 1): solid gray, a reference",
    "legend(\"topright\", c(\"correlation matrix (unreduced)\", \"reduced correlation matrix (factor analytic)\"),",
    "       pch = c(16, 17), lty = c(1, 2))"
  )
}

#' The parallel analysis figure
#'
#' The empirical factor-analytic eigenvalues and the selected reference
#' series, told apart by line type and point shape as well as color, with
#' a legend that names each series; the theorized number and the recorded
#' decision as vertical lines when there are any; the line at 1 for
#' orientation only.
#'
#' @param empirical The empirical factor-analytic eigenvalues.
#' @param reference The reference eigenvalues read against them (the same
#'   length, or `NULL` when none is available).
#' @param reference_label What the reference is (the label of
#'   `sieve_pa_select()`).
#' @param basis_used The correlations the eigenvalues were computed from.
#' @param palette The palette name (see `sieve_palette_colors()`).
#' @param theory_k,decision_k The theorized number of factors and the
#'   number the researcher recorded, or `NULL`.
#' @param shown The ranks to draw, `c(from, to)`, or `NULL` for all (see
#'   `sieve_rank_window()`); nothing is recomputed.
#' @return A ggplot object.
#' @author Ken Kelley
#' @noRd
sieve_pa_plot <- function(empirical, reference = NULL, reference_label = "reference not available",
                          basis_used = "pearson", palette = "nd", theory_k = NULL, decision_k = NULL,
                          shown = NULL) {
  emp <- as.numeric(empirical)
  n_all <- length(emp)
  window <- sieve_rank_window(n_all, shown)
  lines_in <- sieve_rank_window_lines(window, theory_k, decision_k)
  theory_k <- lines_in$theory_k
  decision_k <- lines_in$decision_k
  cols <- sieve_palette_colors(palette, 5L)
  emp_label <- sprintf(
    "Empirical factor-analytic eigenvalues (maximum likelihood, %s correlations)",
    sieve_retention_basis_label(basis_used)
  )
  ref_label <- sprintf("Reference: %s", reference_label)
  long <- data.frame(k = seq_along(emp), eigenvalue = emp, series = emp_label, stringsAsFactors = FALSE)
  if (!is.null(reference) && length(reference)) {
    ref <- as.numeric(reference)[seq_along(emp)]
    long <- rbind(long, data.frame(
      k = seq_along(ref), eigenvalue = ref, series = ref_label,
      stringsAsFactors = FALSE
    ))
  }
  long <- long[is.finite(long$eigenvalue) & long$k >= window[1L] & long$k <= window[2L], , drop = FALSE]
  long$series <- factor(long$series, levels = c(emp_label, ref_label))
  ylim <- sieve_rank_window_ylim(long$eigenvalue, window, n_all)
  sub <- paste(c(
    paste0(
      "Parallel analysis counts the factors whose empirical eigenvalue exceeds the reference in ",
      "order, up to the first that falls below it. Solid gray line at 1: orientation only; Kaiser's ",
      "rule concerns the unreduced correlation matrix (see the Scree plot panel) and is not used."
    ),
    sieve_rank_window_text(window, n_all),
    if (!sieve_rank_window_has_one(ylim)) "The line at 1 lies outside the range of the eigenvalues shown.",
    lines_in$outside
  ), collapse = " ")
  gg <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .data$k, y = .data$eigenvalue, color = .data$series,
      linetype = .data$series, shape = .data$series, group = .data$series
    )
  ) +
    # Solid, gray, and thinner than the series: a reference for orientation.
    ggplot2::geom_hline(yintercept = 1, linetype = "solid", color = "#8a929a", linewidth = 0.45) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_color_manual(
      values = stats::setNames(cols[1:2], c(emp_label, ref_label)),
      name = NULL, drop = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(c("solid", "longdash"), c(emp_label, ref_label)),
      name = NULL, drop = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = stats::setNames(c(16, 17), c(emp_label, ref_label)),
      name = NULL, drop = FALSE
    )
  gg <- sieve_rank_window_vlines(gg, window, long$eigenvalue, theory_k, decision_k)
  if (!is.null(ylim)) {
    gg <- gg + ggplot2::coord_cartesian(ylim = ylim)
  }
  gg +
    ggplot2::scale_x_continuous(breaks = sieve_rank_breaks(window[2L], window[1L])) +
    ggplot2::labs(
      x = "Factor",
      y = "Factor-analytic eigenvalue (reduced correlation matrix)",
      subtitle = paste(strwrap(sub, width = 120), collapse = "\n")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.key.width = grid::unit(2.2, "lines"),
      plot.subtitle = ggplot2::element_text(color = "#5b6770", size = 10)
    )
}

#' Distinguish EM Estimation Respondents From the Exploratory Reference N
#' @param basis A resolved correlation basis.
#' @return Display text.
#' @author Ken Kelley
#' @noRd
sieve_basis_n_text <- function(basis) {
  if (identical(basis$missing, "fiml")) {
    sprintf("EM/FIML basis from %d available respondents; complete-case reference N = %d", basis$n_used, basis$n)
  } else sprintf("n = %d complete cases", basis$n)
}

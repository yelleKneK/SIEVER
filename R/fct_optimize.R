# fct_optimize.R
# The cross-construct item set search. Given the included item pool, this
# lists every item set that can be reached by dropping items within the
# researcher's content constraints and, for each one, computes descriptive
# statistics: reliability, global fit of the measurement model, and simple
# structure. The search proposes candidates; it does not choose one.
#
# Three kinds of rule are kept apart, because they answer different questions.
#
# 1. Content constraints belong to the researcher and are enforced when the
#    candidates are generated and asserted again on every candidate returned
#    (sieve_check_candidate()): the items that must be kept, the smallest
#    number of items each construct (and optionally each facet) must retain,
#    and the largest number of items that may be dropped per construct.
#    The current item set is always the first candidate, and it is the only
#    candidate when no item may be dropped. A construct that already has
#    fewer items than its minimum is never pruned and never removed: it is
#    held intact in every candidate and reported as held.
# 2. Estimation requirements belong to the statistics, not to the researcher:
#    the one-factor model behind omega needs three indicators, a factor in a
#    correlated factors model needs two (sieve_optimizer_requirements()).
#    They never remove a candidate. They decide whether a statistic exists
#    for a candidate, and a candidate without it is listed with the reason
#    and is not ranked on that statistic. A statistic that exists but comes
#    from an improper solution (a negative variance estimate in the
#    confirmatory model, or in the one-factor model behind an omega) is kept
#    as estimated, marked, and not ranked either: a number coming back is not
#    enough to make it usable.
# 3. The capacity limit belongs to the session. The number of candidates is
#    the product over constructs of sums of binomial coefficients, so it is
#    counted algebraically (sieve_optimizer_plan()) and compared with the cap
#    before a single subset is built. A search is either complete or refused;
#    it is never silently truncated. Counting and refusing cost the same
#    whatever maximum to drop is asked for (the work is bounded by the pool),
#    and a count too large to be exact in double precision is reported as
#    approximate.
#
# What the statistics are. Every candidate gets a confirmatory fit with the
# estimator the CFA tab would use for those items. The reliability and the
# simple structure columns are computed from the item codes treated as
# continuous, on complete cases, whatever the confirmed item types are: they
# are NOT in general the coefficients the Reliability and EFA tabs report
# (which use categorical omega for ordered or binary items and can use full
# information estimation with incomplete data). sieve_optimizer_metrics()
# states the target and the sample of each column, and the panel shows those
# labels. Information criteria (AIC, BIC) are not returned: candidates model
# different sets of observed variables, so their likelihoods refer to
# different data and cannot be compared (Burnham & Anderson, 2002).
#
# All statistics are computed on the data used to choose among candidates, so
# they are descriptive and selection dependent (MacCallum, Roznowski, &
# Necowitz, 1992). A chosen item set is a hypothesis to be evaluated on
# independent data (Cudeck & Browne, 1983).

#' The smallest numbers of indicators the optimizer's statistics need
#'
#' These are estimation requirements, a property of each statistic. They are
#' deliberately separate from the researcher's content minimums (see
#' `sieve_optimizer_constraints()`): a content minimum decides which
#' candidates exist, an estimation requirement decides whether a statistic
#' can be computed for a candidate that exists.
#'
#' * `omega`: three. Omega comes from a one-factor model of one construct's
#'   items. With three indicators that model is just identified; with two it
#'   is not identified unless the loadings are constrained equal, which is a
#'   different (essentially tau equivalent) model (McDonald, 1999).
#' * `cfa_factor`: two. A construct with one kept item cannot define a
#'   factor and is left out of the confirmatory model. A factor with two
#'   indicators is identified in a correlated factors model only through its
#'   covariances with the other factors (the two indicator rule; Bollen,
#'   1989), so it can still fail empirically when those covariances are near
#'   zero, and a model with a single factor needs three. Those cases are
#'   read from the fitted model (`sieve_fit_status()`), not assumed here.
#'
#' @return A named list of integers.
#' @author Ken Kelley
#' @noRd
sieve_optimizer_requirements <- function() {
  list(omega = 3L, cfa_factor = 2L)
}

#' The researcher's content constraints for the item set search
#'
#' Validates and stores the constraints that define which candidate item sets
#' exist. All of them limit what may be dropped, so the current item set
#' always satisfies them.
#'
#' @param max_drop Largest number of items that may be dropped from any one
#'   construct (a whole number, zero or more). Zero means no item may be
#'   dropped, so the search returns the current item set and nothing else.
#' @param min_keep Content coverage: the smallest number of items each
#'   construct must retain (a whole number, one or more). Either one number
#'   used for every construct, or a named vector with one entry per
#'   construct.
#' @param min_facet Optional content coverage within constructs: the smallest
#'   number of items each facet must retain. `NULL`, `NA`, or zero means no
#'   facet minimum. Items without a facet carry no facet minimum.
#' @param must_keep Optional character vector of items that every candidate
#'   must retain.
#' @param undersized What to do with a construct (or facet) that already has
#'   fewer items than its minimum. `"hold"` (the default) keeps all of its
#'   items in every candidate and reports it as held; `"refuse"` treats the
#'   minimum as a requirement the pool must already meet and refuses the
#'   search, naming the constructs.
#' @return A list of class `sieve_optimizer_constraints` with the validated
#'   `max_drop`, `min_keep`, `min_facet` (`NULL` when none), `must_keep`
#'   (possibly empty), and `undersized`. Invalid values stop with an error.
#' @author Ken Kelley
#' @noRd
sieve_optimizer_constraints <- function(max_drop = 1L, min_keep = 3L, min_facet = NULL,
                                        must_keep = NULL,
                                        undersized = c("hold", "refuse")) {
  undersized <- match.arg(undersized)
  whole <- function(x, lowest) {
    is.numeric(x) && length(x) >= 1L && all(is.finite(x)) && all(x >= lowest) &&
      all(x == floor(x))
  }
  # A count of items larger than R can index is not a number of items. It is
  # refused with that reason, not coerced (the coercion would give NA).
  too_large <- function(x, what) {
    if (any(x > .Machine$integer.max)) {
      stop(
        sprintf(paste0(
          "%s is too large to be a number of items (the largest usable value ",
          "is %s)."
        ), what, format(.Machine$integer.max, big.mark = ",")),
        call. = FALSE
      )
    }
  }
  if (!whole(max_drop, 0) || length(max_drop) != 1L) {
    stop("The maximum number of items to exclude per construct must be a single whole number, zero or more.",
      call. = FALSE
    )
  }
  too_large(max_drop, "The maximum number of items to exclude per construct")
  named <- !is.null(names(min_keep))
  if (!whole(min_keep, 1) || (!named && length(min_keep) != 1L)) {
    stop(
      paste0(
        "The minimum number of items to keep per construct must be a whole number, ",
        "one or more: a single value, or one named value per construct."
      ),
      call. = FALSE
    )
  }
  too_large(min_keep, "The minimum number of items to keep per construct")
  if (named && (anyNA(names(min_keep)) || any(!nzchar(names(min_keep))) ||
    anyDuplicated(names(min_keep)))) {
    stop("A named minimum must name each construct exactly once.", call. = FALSE)
  }
  if (is.null(min_facet) || (length(min_facet) == 1L && (is.na(min_facet) || min_facet == 0))) {
    min_facet <- NULL
  } else if (!whole(min_facet, 1) || length(min_facet) != 1L) {
    stop("The minimum number of items to keep per facet must be a single whole number, or empty for none.",
      call. = FALSE
    )
  } else {
    too_large(min_facet, "The minimum number of items to keep per facet")
  }
  if (is.null(must_keep)) {
    must_keep <- character(0)
  }
  if (!is.character(must_keep)) {
    stop("The must-keep items must be given as item names.", call. = FALSE)
  }
  must_keep <- unique(must_keep[!is.na(must_keep) & nzchar(must_keep)])
  structure(
    list(
      max_drop = as.integer(max_drop),
      min_keep = if (named) {
        stats::setNames(as.integer(min_keep), names(min_keep))
      } else {
        as.integer(min_keep)
      },
      min_facet = if (is.null(min_facet)) NULL else as.integer(min_facet),
      must_keep = must_keep,
      undersized = undersized
    ),
    class = "sieve_optimizer_constraints"
  )
}

#' The content constraints in words
#'
#' One sentence fragment used wherever the constraints of a search are
#' reported (above the results and in the reproducible code log), so the
#' record always states all of them.
#'
#' @param constraints Constraints from `sieve_optimizer_constraints()`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_constraints_text <- function(constraints) {
  paste(c(
    sprintf("exclude at most %d per construct", constraints$max_drop),
    if (is.null(names(constraints$min_keep))) {
      sprintf("keep at least %d per construct", constraints$min_keep)
    } else {
      sprintf(
        "keep at least %s",
        paste(sprintf("%d in %s", constraints$min_keep, names(constraints$min_keep)),
          collapse = ", "
        )
      )
    },
    if (is.null(constraints$min_facet)) {
      "no facet minimum"
    } else {
      sprintf("keep at least %d per facet", constraints$min_facet)
    },
    if (length(constraints$must_keep)) {
      sprintf("always keep %s", paste(constraints$must_keep, collapse = ", "))
    } else {
      "no must-keep items"
    },
    sprintf(
      if (identical(constraints$undersized, "refuse")) {
        "a construct%s below its minimum stops the search"
      } else {
        "a construct%s below its minimum is held intact"
      },
      if (is.null(constraints$min_facet)) "" else " or facet"
    )
  ), collapse = "; ")
}

#' A number of candidate item sets in words
#'
#' The count is carried as a double. It is stated digit for digit when the
#' arithmetic that produced it was exact (see `sieve_optimizer_plan()`), as
#' an approximation to three significant digits when it was not, and as
#' beyond the range of a double when it overflowed. A display helper: the
#' stored count is never rounded.
#'
#' @param total The number of candidates.
#' @param exact Whether `total` is known to be exact.
#' @return A length one character string, such as `"268,800"`,
#'   `"about 6.36e+17"`, or `"more than 1.79e+308"`.
#' @author Ken Kelley
#' @noRd
sieve_count_text <- function(total, exact = TRUE) {
  if (!is.finite(total)) {
    # The largest double is about 1.797e+308.
    return("more than 1.79e+308")
  }
  if (isTRUE(exact)) {
    return(format(total, big.mark = ",", scientific = FALSE, trim = TRUE))
  }
  paste("about", formatC(total, format = "e", digits = 2))
}

#' Which constructs and facets are held, in sentences
#'
#' A construct or a facet that already has fewer items than its minimum is
#' held: all of its items are in every candidate. The same sentences are
#' used before the search (the live count), above the results, and in the
#' result's notes, so a held construct and a held facet are reported alike.
#'
#' @param constructs,facets The plan's (or a search result's) reports.
#' @param link The words between the name and its state: `" is "` in running
#'   text, `": "` in a list of notes.
#' @return A character vector with one sentence per held construct and per
#'   held facet, empty when nothing is held.
#' @author Ken Kelley
#' @noRd
sieve_held_text <- function(constructs, facets, link = " is ") {
  out <- character(0)
  if (!is.null(constructs) && nrow(constructs)) {
    held <- constructs[constructs$state == "held", , drop = FALSE]
    out <- c(out, sprintf("%s%s%s.", held$construct, link, held$detail))
  }
  if (!is.null(facets) && nrow(facets)) {
    held <- facets[facets$held, , drop = FALSE]
    out <- c(out, sprintf(
      "Facet %s of %s%sheld: too few items to exclude any (%d item%s, facet minimum %d).",
      held$facet, held$construct, link, held$n_items, ifelse(held$n_items == 1L, "", "s"),
      held$min_facet
    ))
  }
  out
}

#' The included pool as the optimizer reads it
#'
#' One row per included item, in ledger order, with the construct and facet
#' used for grouping. A blank or missing construct is grouped as
#' "Unassigned" (the ledger's own convention); a blank facet is no facet.
#'
#' @param meta The item ledger.
#' @return A data frame with `item`, `construct`, and `facet` (`NA` when the
#'   item has no facet).
#' @author Ken Kelley
#' @noRd
sieve_optimizer_pool <- function(meta) {
  empty <- data.frame(
    item = character(0), construct = character(0),
    facet = character(0), stringsAsFactors = FALSE
  )
  if (is.null(meta) || !all(c("item", "construct", "status") %in% names(meta))) {
    return(empty)
  }
  active <- meta[!is.na(meta$status) & meta$status == "active", , drop = FALSE]
  if (!nrow(active)) {
    return(empty)
  }
  construct <- as.character(active$construct)
  construct[is.na(construct) | !nzchar(trimws(construct))] <- "Unassigned"
  facet <- if ("facet" %in% names(active)) as.character(active$facet) else NA_character_
  facet <- rep_len(facet, nrow(active))
  facet[!is.na(facet) & !nzchar(trimws(facet))] <- NA_character_
  data.frame(
    item = as.character(active$item), construct = construct, facet = facet,
    stringsAsFactors = FALSE
  )
}

#' The minimum for each construct in the pool
#'
#' @param min_keep The validated `min_keep` (one number, or named by
#'   construct).
#' @param constructs Character vector of the constructs in the pool.
#' @return A list with `value` (a named integer vector, one entry per
#'   construct, or `NULL`), `absent` (constructs given a minimum that have no
#'   included items), and `unnamed` (constructs in the pool without one).
#' @author Ken Kelley
#' @noRd
sieve_resolve_min_keep <- function(min_keep, constructs) {
  if (is.null(names(min_keep))) {
    return(list(
      value = stats::setNames(rep(as.integer(min_keep), length(constructs)), constructs),
      absent = character(0), unnamed = character(0)
    ))
  }
  absent <- setdiff(names(min_keep), constructs)
  unnamed <- setdiff(constructs, names(min_keep))
  value <- if (length(unnamed)) NULL else stats::setNames(as.integer(min_keep[constructs]), constructs)
  list(value = value, absent = absent, unnamed = unnamed)
}

#' Multiply two counting polynomials, keeping terms up to a degree
#'
#' Coefficient `i` counts the ways to drop `i - 1` items, so the product of
#' two such vectors counts the ways to drop items from two disjoint groups,
#' by the total number dropped.
#'
#' @param a,b Numeric coefficient vectors (first element is degree zero).
#' @param max_degree Highest degree to keep.
#' @return The coefficient vector of the truncated product.
#' @author Ken Kelley
#' @noRd
sieve_poly_product <- function(a, b, max_degree = Inf) {
  out <- numeric(min(length(a) + length(b) - 1L, max_degree + 1))
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      d <- i + j - 1L
      if (d <= length(out)) {
        out[d] <- out[d] + a[i] * b[j]
      }
    }
  }
  out
}

#' Binomial coefficients, exact while they fit in a double
#'
#' \eqn{\binom{m}{0}, \ldots, \binom{m}{u}} by the recurrence
#' \eqn{\binom{m}{j} = \binom{m}{j - 1} (m - j + 1) / j}. The product
#' \eqn{\binom{m}{j - 1} (m - j + 1) = j \binom{m}{j}} is a whole number and
#' the division by \eqn{j} leaves a whole number, so every step is exact in
#' double precision as long as \eqn{j \binom{m}{j}} is below \eqn{2^{53}}.
#' (`choose()` multiplies by the ratios \eqn{(m - j + 1) / j}, which are not
#' whole numbers, and rounds at the end, so it carries no such guarantee for
#' a large result.) Beyond \eqn{2^{53}} the values are approximations, and
#' they overflow to `Inf` like any double.
#'
#' @param m Number of items to choose from (a whole number, zero or more).
#' @param upto Largest subset size wanted (zero to `m`).
#' @return A numeric vector of length `upto + 1`.
#' @author Ken Kelley
#' @noRd
sieve_binomials <- function(m, upto) {
  out <- numeric(upto + 1L)
  out[1L] <- 1
  for (j in seq_len(upto)) {
    out[j + 1L] <- out[j] * (m - j + 1) / j
  }
  out
}

#' Plan the item set search: feasibility, held constructs, exact count
#'
#' Reads the included pool and the constraints and works out, without
#' building any subset, what the search would examine. For one construct
#' with facet groups \eqn{g}, each holding \eqn{m_g} items that may be
#' dropped of which at most \eqn{u_g} may go, the number of ways to drop
#' exactly \eqn{d} items is the coefficient of \eqn{x^d} in
#' \eqn{\prod_g \sum_{j = 0}^{u_g} \binom{m_g}{j} x^j}. The construct
#' contributes the sum of those coefficients up to the most it may lose, and
#' the number of candidates is the product over constructs. The current item
#' set (nothing dropped) is always one of them.
#'
#' A must-keep item is never among the \eqn{m_g}. The most a facet may lose
#' is its size minus its minimum, the most a construct may lose is the
#' smaller of `max_drop` and its size minus its minimum, and a construct or
#' facet smaller than its minimum loses nothing (it is held).
#'
#' The work is bounded by the size of the pool, never by `max_drop`: a
#' `max_drop` larger than any construct allows the same drops as the size of
#' the largest construct. The count is a double. Every quantity formed on
#' the way to it is a whole number no larger than the number of included
#' items times the total (see `sieve_binomials()`), so the count is exact
#' whenever that product is below \eqn{2^{53}}, and `exact` says whether it
#' is. A larger count is an approximation and is reported as one.
#'
#' @param meta The item ledger.
#' @param constraints Constraints from `sieve_optimizer_constraints()`.
#' @return A list with `error` (`NULL`, or the plain reason the search cannot
#'   be run as specified), `active` (the included items in ledger order),
#'   `constraints`, `constructs` and `facets` (data frames that report, for
#'   each, its size, minimum, how many items may be dropped, and whether it
#'   is searched, fixed, or held), `groups` (the per-construct structure the
#'   enumerator uses, each with `by_drops`, the number of ways to drop
#'   exactly 0, 1, ... items from that construct), `total` (the number of
#'   candidates, as a double), `exact` (whether `total` is exact),
#'   `most_droppable` (the most items any one construct can lose under these
#'   constraints), and `by_drops` (the number of candidates by total number
#'   of items dropped, starting at zero).
#' @author Ken Kelley
#' @noRd
sieve_optimizer_plan <- function(meta, constraints = sieve_optimizer_constraints()) {
  pool <- sieve_optimizer_pool(meta)
  plan <- list(
    error = NULL, active = pool$item, constraints = constraints,
    constructs = NULL, facets = NULL, groups = NULL, total = 0,
    exact = TRUE, most_droppable = 0L, by_drops = numeric(0)
  )
  refuse <- function(...) {
    plan$error <- paste0(...)
    plan
  }
  quote_all <- function(x) paste(x, collapse = ", ")
  if (!nrow(pool)) {
    return(refuse("No items are included, so there is nothing to search."))
  }
  if (anyDuplicated(pool$item)) {
    return(refuse(
      "The item ledger lists an included item more than once (",
      quote_all(unique(pool$item[duplicated(pool$item)])), ")."
    ))
  }

  # (a) Items that must be kept have to be in the included pool.
  stray <- setdiff(constraints$must_keep, pool$item)
  if (length(stray)) {
    excluded <- intersect(stray, as.character(meta$item))
    unknown <- setdiff(stray, excluded)
    return(refuse(
      "The must-keep list cannot be honored. ",
      if (length(excluded)) sprintf("Currently excluded: %s. ", quote_all(excluded)) else "",
      if (length(unknown)) sprintf("Not in the item ledger: %s. ", quote_all(unknown)) else "",
      "Include these items, or remove them from the must-keep list."
    ))
  }

  # (b) The minimum for each construct.
  cons <- unique(pool$construct)
  mk <- sieve_resolve_min_keep(constraints$min_keep, cons)
  if (length(mk$absent)) {
    return(refuse(
      "A minimum was set for ", quote_all(mk$absent),
      ", but ", if (length(mk$absent) == 1L) "that construct has" else "those constructs have",
      " no included items, so the minimum cannot be met."
    ))
  }
  if (length(mk$unnamed)) {
    return(refuse(
      "No minimum was given for ", quote_all(mk$unnamed),
      ". Name every construct, or give one minimum for all."
    ))
  }
  min_keep <- mk$value
  min_facet <- constraints$min_facet
  max_drop <- constraints$max_drop

  con_rows <- vector("list", length(cons))
  facet_rows <- list()
  groups <- stats::setNames(vector("list", length(cons)), cons)
  by_drops <- 1
  for (ci in seq_along(cons)) {
    cn <- cons[ci]
    rows <- which(pool$construct == cn)
    its <- pool$item[rows]
    fac <- pool$facet[rows]
    n_c <- length(its)
    k_c <- min_keep[[cn]]
    most <- min(max_drop, n_c - min(k_c, n_c))
    keys <- unique(fac)
    grp <- vector("list", length(keys))
    poly <- 1
    for (gi in seq_along(keys)) {
      key <- keys[gi]
      g_items <- if (is.na(key)) its[is.na(fac)] else its[!is.na(fac) & fac == key]
      n_g <- length(g_items)
      floor_g <- if (!is.na(key) && !is.null(min_facet)) min(min_facet, n_g) else 0L
      droppable <- setdiff(g_items, constraints$must_keep)
      max_out <- min(length(droppable), n_g - floor_g, most)
      grp[[gi]] <- list(facet = key, items = g_items, droppable = droppable, max_out = max_out)
      poly <- sieve_poly_product(poly, sieve_binomials(length(droppable), max_out), most)
      if (!is.na(key)) {
        facet_rows[[length(facet_rows) + 1L]] <- data.frame(
          construct = cn, facet = key, n_items = n_g,
          min_facet = if (is.null(min_facet)) NA_integer_ else min_facet,
          n_must_keep = n_g - length(droppable),
          held = !is.null(min_facet) && n_g < min_facet,
          stringsAsFactors = FALSE
        )
      }
    }
    max_droppable <- length(poly) - 1L
    held <- n_c < k_c
    state <- if (held) "held" else if (max_droppable == 0L) "fixed" else "searched"
    detail <- if (held) {
      sprintf(
        "held: too few items to exclude any (%d item%s, minimum %d)",
        n_c, if (n_c == 1L) "" else "s", k_c
      )
    } else if (max_droppable > 0L) {
      sprintf("searched: up to %d of its %d items may be dropped", max_droppable, n_c)
    } else if (max_drop == 0L) {
      "fixed: no item may be dropped (the maximum to drop is 0)"
    } else if (n_c == k_c) {
      sprintf("fixed: already at the minimum of %d items", k_c)
    } else {
      "fixed: the must-keep items and the facet minimums leave nothing to drop"
    }
    con_rows[[ci]] <- data.frame(
      construct = cn, n_items = n_c, min_keep = k_c,
      n_must_keep = sum(its %in% constraints$must_keep),
      max_droppable = max_droppable, n_subsets = sum(poly),
      state = state, detail = detail, stringsAsFactors = FALSE
    )
    groups[[cn]] <- list(
      construct = cn, items = its, most = max_droppable, groups = grp,
      by_drops = poly
    )
    by_drops <- sieve_poly_product(by_drops, poly)
  }
  plan$constructs <- do.call(rbind, con_rows)
  plan$facets <- if (length(facet_rows)) {
    do.call(rbind, facet_rows)
  } else {
    data.frame(
      construct = character(0), facet = character(0), n_items = integer(0),
      min_facet = integer(0), n_must_keep = integer(0), held = logical(0),
      stringsAsFactors = FALSE
    )
  }
  plan$groups <- groups

  # Under the "refuse" policy a minimum is a requirement on the pool itself.
  if (identical(constraints$undersized, "refuse")) {
    short_c <- plan$constructs[plan$constructs$state == "held", , drop = FALSE]
    short_f <- plan$facets[plan$facets$held, , drop = FALSE]
    if (nrow(short_c) || nrow(short_f)) {
      what <- c(
        sprintf(
          "construct %s has %d item%s but its minimum is %d", short_c$construct,
          short_c$n_items, ifelse(short_c$n_items == 1L, "", "s"), short_c$min_keep
        ),
        sprintf(
          "facet %s of %s has %d item%s but the facet minimum is %d", short_f$facet,
          short_f$construct, short_f$n_items, ifelse(short_f$n_items == 1L, "", "s"),
          short_f$min_facet
        )
      )
      return(refuse(
        "The included items cannot meet the content minimums (",
        paste(what, collapse = "; "),
        "). Lower the minimum, include more items there, or choose to hold such ",
        "constructs intact."
      ))
    }
  }

  plan$total <- prod(plan$constructs$n_subsets)
  plan$exact <- is.finite(plan$total) && plan$total * nrow(pool) < 2^53
  plan$most_droppable <- max(plan$constructs$max_droppable)
  plan$by_drops <- by_drops
  plan
}

#' The number of candidates under each smaller maximum to drop
#'
#' Read from the plan that is already in hand, without planning again. The
#' number of ways to drop exactly \eqn{j} items from a construct does not
#' depend on the maximum as long as \eqn{j} is within it (a set of \eqn{j}
#' dropped items takes at most \eqn{j} from any facet), so the count under a
#' smaller maximum \eqn{d} is the product over constructs of the first
#' \eqn{d + 1} of those numbers, summed. The table stops at the most any
#' construct can lose: every larger maximum gives the plan's own total.
#'
#' @param plan A feasible plan from `sieve_optimizer_plan()`.
#' @return A data frame with `max_drop` (0 up to one less than
#'   `plan$most_droppable`) and `total`.
#' @author Ken Kelley
#' @noRd
sieve_plan_alternatives <- function(plan) {
  running <- lapply(plan$groups, function(g) cumsum(g$by_drops))
  fewer <- seq_len(plan$most_droppable) - 1L
  data.frame(
    max_drop = fewer,
    total = vapply(fewer, function(d) {
      prod(vapply(running, function(cs) cs[min(d + 1L, length(cs))], numeric(1)))
    }, numeric(1))
  )
}
#' All subsets of a given size
#'
#' The one place the optimizer builds combinations, so a test can confirm
#' that a refused search never reaches it.
#'
#' @param x A vector.
#' @param m Subset size.
#' @return A list of vectors.
#' @author Ken Kelley
#' @noRd
sieve_combn <- function(x, m) {
  utils::combn(x, m, simplify = FALSE)
}

#' Every allowed set of items to drop from one construct
#'
#' @param construct One element of the plan's `groups`.
#' @return A list of character vectors, the empty set first, then by size.
#' @author Ken Kelley
#' @noRd
sieve_construct_drop_sets <- function(construct) {
  sets <- list(character(0))
  for (g in construct$groups) {
    options <- list(character(0))
    for (j in seq_len(g$max_out)) {
      options <- c(options, sieve_combn(g$droppable, j))
    }
    grown <- list()
    for (s in sets) {
      for (o in options) {
        if (length(s) + length(o) <= construct$most) {
          grown[[length(grown) + 1L]] <- c(s, o)
        }
      }
    }
    sets <- grown
  }
  sets[order(lengths(sets))]
}

#' Build every candidate item set of a plan
#'
#' Materializes the candidates the plan counted. This is the only step that
#' allocates in proportion to the number of candidates, so the search calls
#' it only after the count has been compared with the cap.
#'
#' @param plan A feasible plan from `sieve_optimizer_plan()`.
#' @return A list of kept-item vectors, each in ledger order. The first is
#'   the current item set; the rest follow by increasing number of items
#'   dropped.
#' @author Ken Kelley
#' @noRd
sieve_enumerate_candidates <- function(plan) {
  per <- lapply(plan$groups, sieve_construct_drop_sets)
  sizes <- lengths(per)
  total <- prod(sizes)
  dropped <- vector("list", total)
  index <- rep(1L, length(per))
  for (i in seq_len(total)) {
    dropped[[i]] <- unlist(Map(function(sets, j) sets[[j]], per, index), use.names = FALSE)
    # Advance the mixed radix counter (last construct fastest).
    pos <- length(index)
    while (pos >= 1L) {
      if (index[pos] < sizes[pos]) {
        index[pos] <- index[pos] + 1L
        break
      }
      index[pos] <- 1L
      pos <- pos - 1L
    }
  }
  dropped <- dropped[order(lengths(dropped))]
  lapply(dropped, function(d) plan$active[!plan$active %in% d])
}

#' Does an item set satisfy the researcher's content constraints?
#'
#' Checks one kept-item set directly against the current ledger and the
#' constraints, without reference to how the set was generated. The search
#' applies it to every candidate it returns, and the step that applies a
#' chosen candidate to the ledger can call it again, so an item set that
#' would remove a construct, drop a must-keep item, or fall below a content
#' minimum is caught at both ends.
#'
#' @param kept Character vector of the items the candidate keeps.
#' @param meta The item ledger the candidate would be applied to.
#' @param constraints Constraints from `sieve_optimizer_constraints()` (the
#'   `constraints` element of a search result).
#' @return A list with `ok` (`TRUE` when every constraint holds) and
#'   `violations` (a character vector of plain sentences, empty when `ok`).
#' @author Ken Kelley
#' @noRd
sieve_check_candidate <- function(kept, meta, constraints = sieve_optimizer_constraints()) {
  pool <- sieve_optimizer_pool(meta)
  bad <- character(0)
  kept <- as.character(kept)
  if (!length(kept)) {
    bad <- c(bad, "The item set keeps no items.")
  }
  if (anyDuplicated(kept)) {
    bad <- c(bad, sprintf(
      "The item set lists an item more than once (%s).",
      paste(unique(kept[duplicated(kept)]), collapse = ", ")
    ))
  }
  stray <- setdiff(kept, pool$item)
  if (length(stray)) {
    bad <- c(bad, sprintf(
      "The item set keeps items that are not currently included (%s).",
      paste(stray, collapse = ", ")
    ))
  }
  lost <- setdiff(constraints$must_keep, kept)
  if (length(lost)) {
    bad <- c(bad, sprintf(
      "The item set does not keep the must-keep item%s %s.",
      if (length(lost) == 1L) "" else "s", paste(lost, collapse = ", ")
    ))
  }
  cons <- unique(pool$construct)
  mk <- sieve_resolve_min_keep(constraints$min_keep, cons)
  if (length(mk$absent) || length(mk$unnamed)) {
    bad <- c(bad, sprintf(
      "The construct minimums do not match the included constructs (%s).",
      paste(c(mk$absent, mk$unnamed), collapse = ", ")
    ))
  }
  for (cn in cons) {
    its <- pool$item[pool$construct == cn]
    n_c <- length(its)
    n_kept <- sum(its %in% kept)
    if (!is.null(mk$value)) {
      k_c <- mk$value[[cn]]
      if (n_c < k_c && identical(constraints$undersized, "refuse")) {
        bad <- c(bad, sprintf(
          "Construct %s has %d item%s but its minimum is %d.",
          cn, n_c, if (n_c == 1L) "" else "s", k_c
        ))
      }
      if (n_kept < min(k_c, n_c)) {
        bad <- c(bad, sprintf(
          "Construct %s would keep %d of its %d item%s; at least %d must be kept%s.",
          cn, n_kept, n_c, if (n_c == 1L) "" else "s", min(k_c, n_c),
          if (n_c < k_c) " (it is below its minimum, so it is held intact)" else ""
        ))
      }
    }
    if (n_c - n_kept > constraints$max_drop) {
      bad <- c(bad, sprintf(
        "The item set drops %d items from %s; at most %d may be dropped per construct.",
        n_c - n_kept, cn, constraints$max_drop
      ))
    }
    if (!is.null(constraints$min_facet)) {
      fac <- pool$facet[pool$construct == cn]
      for (key in unique(fac[!is.na(fac)])) {
        f_items <- its[!is.na(fac) & fac == key]
        n_f <- length(f_items)
        if (n_f < constraints$min_facet && identical(constraints$undersized, "refuse")) {
          bad <- c(bad, sprintf(
            "Facet %s of %s has %d item%s but the facet minimum is %d.",
            key, cn, n_f, if (n_f == 1L) "" else "s", constraints$min_facet
          ))
        }
        if (sum(f_items %in% kept) < min(constraints$min_facet, n_f)) {
          bad <- c(bad, sprintf(
            "Facet %s of %s would keep %d of its %d item%s; at least %d must be kept.",
            key, cn, sum(f_items %in% kept), n_f, if (n_f == 1L) "" else "s",
            min(constraints$min_facet, n_f)
          ))
        }
      }
    }
  }
  list(ok = !length(bad), violations = bad)
}

#' Mean item complexity and hyperplane proportion for a candidate item set
#'
#' A compact simple structure summary from one exploratory factor analysis of
#' the supplied rows by `DMAR::fa_exploratory()`: maximum likelihood on the
#' complete rows (`missing = "listwise"`), direct oblimin rotation (none at
#' one factor), the item codes read as continuous, and as many factors as
#' there are constructs. Item complexity is Hofmann's (1978) index (1 for an
#' item that loads on a single factor, larger as its loadings spread), as
#' DMAR reports it; the hyperplane proportion is the share of near zero
#' pattern loadings (absolute value below 0.10), which a clean simple
#' structure makes large (Thurstone, 1947). The correlations are Pearson
#' whatever the confirmed item types are, so for ordered or binary items this
#' is not the analysis the EFA tab runs. DMAR refuses an improper solution (a
#' residual variance below zero), one that did not converge, and a
#' likelihood without a maximum, by class, with its reason; the statistics
#' are then unavailable, with that reason.
#'
#' The oblique rotation is started from 30 random orientations, so it draws
#' random numbers. DMAR runs it on a local stream seeded with `seed` (under
#' the generator in force) and restores the caller's stream, so the result
#' can be replayed.
#'
#' @param X Complete-case data frame of the kept item columns.
#' @param k Number of factors to extract (the number of constructs).
#' @param seed The seed of the local stream (a single whole number), or
#'   `NULL`.
#' @return A list with `complexity` (mean) and `hyperplane` (proportion),
#'   both `NA` when the analysis is unavailable; `n` (the rows analyzed);
#'   `reason` (why it is unavailable, or `NULL`); `warnings` (what the
#'   engine signaled); and `seconds` (the time of the DMAR call).
#' @author Ken Kelley
#' @noRd
sieve_simple_structure <- function(X, k, seed) {
  out <- list(
    complexity = NA_real_, hyperplane = NA_real_, n = nrow(X),
    reason = NULL, warnings = character(0), seconds = NA_real_
  )
  unavailable <- function(why) {
    out$reason <- why
    out
  }
  if (k < 1L || ncol(X) <= k) {
    return(unavailable(sprintf(
      "%d item%s cannot define %d factor%s", ncol(X),
      if (ncol(X) == 1L) "" else "s", k, if (k == 1L) "" else "s"
    )))
  }
  if (nrow(X) <= ncol(X)) {
    return(unavailable(sprintf(
      "only %d complete case%s for %d items", nrow(X),
      if (nrow(X) == 1L) "" else "s", ncol(X)
    )))
  }
  if ((ncol(X) - k)^2 < ncol(X) + k) {
    return(unavailable(sprintf(
      "an exploratory model of %d factors is not identified with %d items (negative degrees of freedom)",
      k, ncol(X)
    )))
  }
  record <- sieve_efa_dmar_run(sieve_efa_point_call(k, data = X, seed = seed))
  out$seconds <- record$seconds
  read <- sieve_efa_point_read(record)
  out$warnings <- read$warnings
  if (!isTRUE(read$ok)) {
    return(unavailable(paste0("the exploratory factor analysis gave no solution: ", read$status$message)))
  }
  out$complexity <- mean(read$complexity, na.rm = TRUE)
  out$hyperplane <- mean(abs(read$loadings) < 0.10, na.rm = TRUE)
  if (!is.finite(out$complexity) || !is.finite(out$hyperplane)) {
    return(unavailable("the exploratory factor analysis returned no usable loadings"))
  }
  out
}

#' What each optimizer statistic estimates, and on which rows
#'
#' The single description of the optimizer's columns, read by the panel for
#' its labels and by the ordering control for its choices, so the words on
#' screen cannot drift from what is computed. Every statistic names its
#' target (the quantity and the model that produced it) and its sample (the
#' rows it was computed on). All samples are rows of the data being
#' searched.
#'
#' @return A data frame with `metric` (column name in the results table),
#'   `label`, `family` (`"reliability"`, `"fit"`, or `"structure"`),
#'   `better` (`"higher"` or `"lower"`: the direction used when the
#'   researcher orders by it), `target`, and `sample`.
#' @author Ken Kelley
#' @noRd
sieve_optimizer_metrics <- function() {
  omega_target <- paste0(
    "Coefficient Omega, \u03c9, of each construct's kept items, from a one-factor model fit to the ",
    "item codes treated as continuous (model implied total variance)"
  )
  omega_sample <- "cases complete on that construct's kept items"
  fit_target <- paste0(
    "of the correlated factors confirmatory model of the candidate's kept items, from the ",
    "scaled test statistic where the estimator reports one"
  )
  fit_sample <- paste0(
    "rows used by that fit: with continuous items, every row with at least one observed ",
    "kept item (full information maximum likelihood); with ordered or binary items, ",
    "complete rows (WLSMV)"
  )
  ss_target <- paste0(
    "from one exploratory factor analysis of the kept items (DMAR::fa_exploratory, maximum ",
    "likelihood, direct oblimin from random starts on a recorded seed, Pearson correlations, as ",
    "many factors as constructs)"
  )
  ss_sample <- "cases complete on all kept items"
  data.frame(
    metric = c("omega_min", "omega_mean", "cfi", "rmsea", "srmr", "complexity", "hyperplane"),
    label = c("min robust omega", "mean robust omega", "CFI", "RMSEA", "SRMR", "complexity", "hyperplane"),
    family = c("reliability", "reliability", "fit", "fit", "fit", "structure", "structure"),
    better = c("higher", "higher", "higher", "lower", "lower", "lower", "higher"),
    target = c(
      paste0("smallest across constructs of ", omega_target),
      paste0("mean across constructs of ", omega_target),
      paste0("comparative fit index ", fit_target),
      paste0("root mean square error of approximation ", fit_target),
      paste0(
        "standardized root mean square residual of the correlated factors confirmatory ",
        "model of the candidate's kept items"
      ),
      paste0("mean Hofmann item complexity ", ss_target),
      paste0("proportion of pattern loadings below 0.10 in absolute value ", ss_target)
    ),
    sample = c(
      omega_sample, omega_sample, fit_sample, fit_sample, fit_sample,
      ss_sample, ss_sample
    ),
    stringsAsFactors = FALSE
  )
}

#' Omega of one construct's kept items, with the state of its solution
#'
#' Robust omega (observed denominator) from DMAR: the squared sum of the
#' one-factor model's loadings over the variance of the unit-weighted
#' composite from the items' covariance matrix (Kelley & Pornprasertmanit,
#' 2016), no interval, with what the engines signaled kept instead of
#' suppressed, and with the solution classified. A number coming back is not enough to call a
#' reliability estimate usable:
#'
#' * `"ok"`: an estimate between 0 and 1 from a solution nothing was said
#'   against.
#' * `"improper"`: the one-factor solution has a negative residual variance
#'   estimate (a Heywood case), or the estimate lies outside 0 to 1, which
#'   no reliability can. The numerator comes from the one-factor solution,
#'   so a negative residual variance lets the squared sum of the loadings
#'   exceed the composite's variance, and omega then exceeds 1. The value is
#'   kept, as estimated, so it can be shown, but it is not a usable
#'   reliability estimate.
#' * `"no_estimate"`: the model failed or did not converge.
#'
#' DMAR returns the estimate without the status of the solution, so the
#' Heywood case is read from the condition DMAR signals for it (and from
#' lavaan's own report of a negative variance). The range check does not
#' depend on any wording.
#'
#' @param X Complete-case data frame of one construct's kept items (three or
#'   more columns).
#' @return A list with `value` (the estimate, or `NA`), `state` (`"ok"`,
#'   `"improper"`, or `"no_estimate"`), `problem` (a plain description of
#'   what is wrong, or `NULL`), and `conditions` (every warning signaled).
#' @author Ken Kelley
#' @noRd
sieve_omega_estimate <- function(X) {
  run <- sieve_capture(
    sieve_one_factor(X, missing = "listwise", fast = TRUE)
  )
  out <- list(
    value = NA_real_, state = "no_estimate", problem = NULL,
    conditions = run$warnings
  )
  if (!run$ok) {
    out$problem <- sub("[.]\\s*$", "", gsub("\\s+", " ", run$error))
    return(out)
  }
  value <- run$value$value[run$value$term == "estimate"]
  value <- if (length(value)) value[1] else NA_real_
  if (!is.finite(value)) {
    out$problem <- "no estimate was returned"
    return(out)
  }
  out$value <- value
  heywood <- any(grepl("Heywood|Improper solution|variances are negative", run$warnings))
  outside <- value < 0 || value > 1
  if (heywood || outside) {
    out$state <- "improper"
    out$problem <- paste(c(
      if (heywood) "an improper solution, with a negative residual variance estimate",
      if (outside) "an omega outside the range 0 to 1"
    ), collapse = " and ")
  } else {
    out$state <- "ok"
  }
  out
}

#' Compute one candidate's statistics and say which of them exist
#'
#' The statistical work of the search for a single kept-item set, as a plain
#' function. Three families are computed and each gets its own availability
#' verdict and reason, so a statistic that cannot be computed is reported as
#' unavailable instead of appearing as a blank:
#'
#' * reliability: Robust omega (observed denominator) for each construct (continuous one-factor model,
#'   complete cases on that construct's kept items), summarized by its
#'   minimum and mean. If any construct lacks an omega (fewer than three
#'   kept items, or no estimate), the summaries are `NA`: a worst case
#'   cannot be stated while one construct's reliability is unknown. If every
#'   construct has an omega but one of them comes from an improper one-factor
#'   solution or lies outside 0 to 1 (`sieve_omega_estimate()`), the
#'   summaries keep their values, as an improper confirmatory solution keeps
#'   its fit indices, but the family is not usable for ordering and the
#'   reason names the construct.
#' * fit: the correlated factors model for the kept items, resolved and run
#'   through the shared specification ([sieve_cfa_spec()],
#'   `sieve_run_cfa()`). It is usable for ordering only when the solution is
#'   converged, identified, admissible, and testable, covers every
#'   construct, and used the same estimator as the current item set.
#' * structure: `sieve_simple_structure()`, when requested.
#'
#' @param items_df Data frame holding the item columns.
#' @param meta The item ledger.
#' @param kept The candidate's kept items.
#' @param simple_structure Whether to compute the simple structure columns.
#' @param reference_estimator The estimator of the current item set's
#'   confirmatory model, or `NULL` when this candidate is the current set.
#' @param seed The seed for the simple structure rotation; see
#'   `sieve_simple_structure()`. Needed only when `simple_structure` is
#'   `TRUE`.
#' @return A list with `row` (a one row data frame of full precision
#'   statistics, sample sizes, availability flags, and reasons), `omega`,
#'   `omega_n`, and `omega_state` (named by construct; the state is `"ok"`,
#'   `"improper"`, `"too_few_items"`, `"no_estimate"`, or `"unavailable"`),
#'   `estimator`, and `warnings` (everything lavaan and DMAR signaled for
#'   this candidate).
#' @author Ken Kelley
#' @noRd
sieve_score_candidate <- function(items_df, meta, kept, simple_structure = TRUE,
                                  reference_estimator = NULL, seed = NULL) {
  need <- sieve_optimizer_requirements()
  pool <- sieve_optimizer_pool(meta)
  cons <- unique(pool$construct)
  kept <- pool$item[pool$item %in% kept]
  kept_con <- pool$construct[pool$item %in% kept]
  counts <- vapply(cons, function(cn) sum(kept_con == cn), integer(1))
  warn <- character(0)
  numeric_rows <- function(cols) {
    X <- items_df[, cols, drop = FALSE]
    X[] <- lapply(X, function(v) suppressWarnings(as.numeric(v)))
    X[stats::complete.cases(X), , drop = FALSE]
  }

  # Reliability: omega per construct, continuous model, complete cases.
  omega <- stats::setNames(rep(NA_real_, length(cons)), cons)
  omega_n <- stats::setNames(rep(NA_integer_, length(cons)), cons)
  omega_state <- stats::setNames(rep("unavailable", length(cons)), cons)
  rel_why <- character(0)
  {
    short <- cons[counts < need$omega]
    omega_state[short] <- "too_few_items"
    if (length(short)) {
      rel_why <- sprintf(
        "omega unavailable (%s; the one-factor model behind omega needs at least %d)",
        paste(sprintf(
          "%s would have %d item%s", short, counts[short],
          ifelse(counts[short] == 1L, "", "s")
        ), collapse = ", "),
        need$omega
      )
    }
    for (cn in setdiff(cons, short)) {
      Xc <- numeric_rows(kept[kept_con == cn])
      omega_n[[cn]] <- nrow(Xc)
      est <- sieve_omega_estimate(Xc)
      omega[[cn]] <- est$value
      omega_state[[cn]] <- est$state
      # What DMAR and lavaan said about this one-factor model stays with the
      # candidate, labeled with the construct it concerns.
      warn <- c(warn, sprintf(
        "omega of %s: %s", rep(cn, length(est$conditions)),
        est$conditions
      ))
      if (identical(est$state, "no_estimate")) {
        rel_why <- c(rel_why, sprintf(
          "omega unavailable (the one-factor model for %s gave no estimate: %s; %d complete case%s)",
          cn, est$problem, nrow(Xc), if (nrow(Xc) == 1L) "" else "s"
        ))
      } else if (identical(est$state, "improper")) {
        rel_why <- c(rel_why, sprintf(
          "omega not ranked (the one-factor model for %s gave %s, so its omega is shown but is not a usable reliability estimate)",
          cn, est$problem
        ))
      }
    }
  }
  rel_ok <- !length(rel_why)
  # One code for the family, the most limiting condition first.
  rel_code <- c("unavailable", "too_few_items", "no_estimate", "improper", "ok")
  rel_code <- rel_code[rel_code %in% omega_state][1]
  # The summaries exist when every construct has an estimate. They are usable
  # for ordering only when `rel_ok` also holds.
  has_all <- all(is.finite(omega))

  # Fit: the correlated factors model, through the shared specification.
  tmp <- meta
  tmp$status <- ifelse(tmp$item %in% kept, "active", "parked")
  spec <- sieve_cfa_spec(tmp)
  run <- sieve_run_cfa(spec, items_df)
  warn <- c(warn, run$warnings)
  code <- run$status$code
  fm <- NULL
  fit_n <- NA_integer_
  if (!is.null(run$fit)) {
    got <- sieve_capture(sieve_fit_measures(run$fit, conf_level = sieve_default_rmsea_conf_level()))
    warn <- c(warn, got$warnings)
    fm <- got$value
    fit_n <- as.integer(sieve_capture(sum(lavaan::lavInspect(run$fit, "nobs")))$value %||%
      NA_integer_)
  }
  usable <- code %in% c("ok", "inadmissible") && !is.null(fm)
  pick <- function(name) {
    if (usable && is.finite(fm[[name]])) unname(fm[[name]]) else NA_real_
  }
  left_out <- cons[counts < need$cfa_factor]
  fit_why <- character(0)
  said <- sub("[.]$", "", run$status$message)
  said <- paste0(tolower(substr(said, 1L, 1L)), substring(said, 2L))
  if (identical(code, "inadmissible")) {
    fit_why <- c(fit_why, paste0("fit not ranked (", said, ")"))
  } else if (!identical(code, "ok")) {
    fit_why <- c(fit_why, paste0("fit unavailable (", said, ")"))
  } else if (isTRUE(run$status$just_identified)) {
    fit_why <- c(fit_why, paste0(
      "fit not ranked (the model is just identified, with zero ",
      "degrees of freedom, so its fit is perfect by construction)"
    ))
  }
  if (length(left_out)) {
    fit_why <- c(fit_why, sprintf(
      "fit not ranked (the confirmatory model omits %s, because fewer than %d items cannot define a factor, so it does not describe the whole item set)",
      paste(left_out, collapse = ", "), need$cfa_factor
    ))
  }
  if (!is.null(reference_estimator) && !identical(spec$estimator, reference_estimator)) {
    fit_why <- c(fit_why, sprintf(
      "fit not ranked (this item set is fit with %s, the current item set with %s)",
      spec$estimator, reference_estimator
    ))
  }
  if (usable) {
    index <- c(cfi = "CFI", rmsea = "RMSEA", srmr = "SRMR")
    absent <- index[!vapply(names(index), function(nm) is.finite(pick(nm)), logical(1))]
    if (length(absent)) {
      fit_why <- c(fit_why, sprintf(
        "fit not ranked (lavaan returned no %s for this model)",
        paste(absent, collapse = ", ")
      ))
    }
  }
  fit_ok <- !length(fit_why)

  # Simple structure: one exploratory analysis of all kept items.
  ss <- if (isTRUE(simple_structure)) {
    sieve_simple_structure(numeric_rows(kept), length(cons), seed)
  } else {
    list(
      complexity = NA_real_, hyperplane = NA_real_, n = NA_integer_,
      reason = NULL, warnings = character(0)
    )
  }
  warn <- c(warn, ss$warnings)
  ss_why <- if (!isTRUE(simple_structure)) {
    "simple structure not requested for this search"
  } else if (is.null(ss$reason)) {
    character(0)
  } else {
    paste0("simple structure unavailable (", ss$reason, ")")
  }
  ss_ok <- !length(ss_why)

  wanted <- c(rel_ok, fit_ok, if (isTRUE(simple_structure)) ss_ok)
  status <- if (all(wanted)) "eligible" else if (any(wanted)) "partly eligible" else "not eligible"
  why <- c(rel_why, fit_why, if (isTRUE(simple_structure)) ss_why)
  dropped <- setdiff(pool$item, kept)
  row <- data.frame(
    n_items = length(kept),
    n_dropped = length(dropped),
    per_construct = paste(sprintf("%s:%d", names(counts), counts), collapse = " "),
    omega_min = if (has_all) min(omega) else NA_real_,
    omega_mean = if (has_all) mean(omega) else NA_real_,
    omega_n_min = if (all(is.na(omega_n))) NA_integer_ else min(omega_n, na.rm = TRUE),
    omega_n_max = if (all(is.na(omega_n))) NA_integer_ else max(omega_n, na.rm = TRUE),
    reliability_code = rel_code,
    cfi = pick("cfi"),
    rmsea = pick("rmsea"),
    rmsea_lo = pick("rmsea_lo"),
    rmsea_hi = pick("rmsea_hi"),
    srmr = pick("srmr"),
    fit_n = fit_n,
    fit_estimator = spec$estimator,
    fit_missing = spec$missing,
    fit_code = code,
    complexity = ss$complexity,
    hyperplane = ss$hyperplane,
    structure_n = ss$n,
    reliability_ok = rel_ok,
    fit_ok = fit_ok,
    structure_ok = ss_ok,
    reliability_note = paste(rel_why, collapse = "; "),
    fit_note = paste(fit_why, collapse = "; "),
    structure_note = paste(ss_why, collapse = "; "),
    status = status,
    reason = paste(why, collapse = "; "),
    dropped = if (length(dropped)) paste(dropped, collapse = ", ") else "(none)",
    stringsAsFactors = FALSE
  )
  list(
    row = row, omega = omega, omega_n = omega_n, omega_state = omega_state,
    estimator = spec$estimator, warnings = unique(warn),
    families = if (usable) attr(fm, "families") else NULL
  )
}

#' Put Candidates in the Order the Researcher Asked for
#'
#' The current item set is always first, as the reference. With
#' `by = "search"` the candidates follow in search order (fewest items
#' dropped first), which privileges no statistic. With a statistic from
#' `sieve_optimizer_metrics()`, the candidates for which that statistic is
#' available are ranked on it (ties share a rank), and every other candidate
#' follows, marked as not ranked. No candidate is ever left out, and a
#' candidate without the statistic is never placed among the ranked ones.
#'
#' @param table The `table` element of a search result.
#' @param by `"search"`, `"omega_min"`, `"omega_mean"`, `"cfi"`, `"rmsea"`,
#'   `"srmr"`, `"complexity"`, or `"hyperplane"`. An unavailable or unrecognized
#'   choice retains search order; no alternative statistic is selected.
#' @return A data frame aligned to the rows of `table` with `id`, `position`
#'   (1 is shown first), `rank` (`NA` unless ranked), `group`
#'   (`"reference"`, `"ranked"`, `"not ranked"`, or `"search order"`), and
#'   `label` (the text shown in the order column).
#' @author Ken Kelley
#' @examples
#' # The columns of a search's table that the order reads, written out: the
#' # current item set and three candidates, the last without a usable fit.
#' candidates <- data.frame(id = 1:4,
#'   role = c("baseline", "candidate", "candidate", "candidate"),
#'   rmsea = c(.092, .061, .078, NA), fit_ok = c(TRUE, TRUE, TRUE, FALSE))
#' sieve_order_candidates(candidates, by = "rmsea")
#' sieve_order_candidates(candidates, by = "search")
#' @export
sieve_order_candidates <- function(table, by = "search") {
  metrics <- sieve_optimizer_metrics()
  n <- nrow(table)
  is_ref <- table$role == "baseline"
  out <- data.frame(
    id = table$id, position = NA_integer_, rank = NA_integer_,
    group = "search order", label = "search order",
    stringsAsFactors = FALSE
  )
  out$group[is_ref] <- "reference"
  out$label[is_ref] <- "reference"
  if (is.null(by) || !by %in% metrics$metric) {
    out$position[order(!is_ref, table$id)] <- seq_len(n)
    return(out)
  }
  m <- metrics[match(by, metrics$metric), ]
  value <- table[[by]]
  eligible <- !is_ref & table[[paste0(m$family, "_ok")]] & is.finite(value)
  key <- if (identical(m$better, "higher")) -value else value
  out$rank[eligible] <- rank(key[eligible], ties.method = "min")
  out$group[eligible] <- "ranked"
  out$label[eligible] <- as.character(out$rank[eligible])
  rest <- !is_ref & !eligible
  out$group[rest] <- "not ranked"
  out$label[rest] <- "not ranked"
  tier <- ifelse(is_ref, 0L, ifelse(eligible, 1L, 2L))
  out$position[order(tier, ifelse(eligible, key, 0), table$id)] <- seq_len(n)
  out
}

#' Search the Item Sets the Constraints Allow and Describe Each One
#'
#' Plans the search (`sieve_optimizer_plan()`), compares the exact number of
#' candidates with `cap` before anything is built, and then either refuses
#' or examines every candidate: there is no partial search. The first
#' candidate is always the exact current item set (the reference), and with
#' `max_drop = 0` it is the only one. Every candidate is checked against the
#' content constraints again before it is returned.
#'
#' The statistics are descriptions of each candidate in the data being
#' searched (see `sieve_optimizer_metrics()` and `sieve_score_candidate()`).
#' The function ranks nothing and applies nothing: ordering is the
#' researcher's choice (`sieve_order_candidates()`), and so is the decision.
#'
#' @details
#' Item values must already reflect intended reverse scoring. Only active
#' ledger items are searched. Coefficients describe these data and are not
#' external validation; no post-selection uncertainty correction is supplied.
#' Model-based quantities assume independent sampling units. No cluster or
#' repeated-observation adjustment is implemented by this search.
#' @param items_df Numeric item-response data frame, with one row per person.
#' @param meta Ledger with `item`, `construct`, `facet`, `type`, and `status`.
#' @param max_drop Largest number dropped per construct, a whole number at
#'   least zero. Zero describes only the current item set.
#' @param min_keep Minimum kept per construct: one positive whole number or
#'   a vector named by construct.
#' @param min_facet Minimum kept within each assigned facet; `NULL`, `NA`, or
#'   zero imposes no facet minimum. Unassigned items have no facet minimum.
#' @param must_keep Item names that every candidate must retain.
#' @param undersized `"hold"` retains every item of an already undersized
#'   construct or facet; `"refuse"` refuses the search and identifies it.
#' @param cap The most candidates the search may examine. A search with more
#'   is refused before any candidate is built or any model is fit.
#' @param simple_structure If `TRUE`, also compute the simple structure
#'   columns (one more exploratory fit per candidate; slower).
#' @param progress Optional function of one argument called once per
#'   candidate (for a progress bar); ignored when `NULL`.
#' @param seed Optional seed for the simple structure rotation, which starts
#'   from random orientations. `NULL` (the default) draws one from the
#'   caller's stream (`sieve_draw_seed()`). The same seed is used for every
#'   candidate, so a candidate's values do not depend on its place in the
#'   search, and the seed used is returned for replay. Not used when
#'   `simple_structure` is `FALSE`.
#' @return When the search ran, a list with `table` (one row per candidate,
#'   full precision, with an availability flag and reason for every family
#'   of statistics and an overall `status` and `reason`), `kept` (the kept
#'   items of each row), `total` and `evaluated` (equal: every candidate was
#'   examined), `complete` (`TRUE`), `constraints`, `constructs` and `facets`
#'   (the plan's reports, including held constructs and held facets),
#'   `omega`, `omega_n`, and `omega_state` (candidate by construct matrices:
#'   the estimate, its complete cases, and the state of its one-factor
#'   solution), `warnings` (per candidate: everything the engines signaled),
#'   `simple_structure` (whether those columns were requested), `rng` (the
#'   seed record of the simple structure rotation, or `NULL`), `elapsed`
#'   (the seconds the search took, wall clock, from the plan to the last
#'   candidate, which the panel uses to say roughly how long the next
#'   search would take), and `notes`.
#'   When the search was refused, a list with `error` (the reason), `total`
#'   (the count, or 0), `evaluated` (0), and, when known, `constraints`,
#'   `constructs`, `facets`, and, for a refusal above the cap, `exact`
#'   (whether the count is exact; see `sieve_optimizer_plan()`) and
#'   `alternatives` (the count for each smaller `max_drop`, up to the most
#'   any construct can lose).
#' @param project Optional project context; enforces its construct-basis policy.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' # Keeping every item but x3 and x6 limits the search to four candidates,
#' # so the example is quick.
#' search <- sieve_optimize_items(items, meta, max_drop = 1L, min_keep = 2L,
#'   must_keep = c("x1", "x2", "x4", "x5", "x7", "x8", "x9"), simple_structure = FALSE)
#' search$total
#' search$table[, c("id", "per_construct", "cfi", "rmsea", "rmsea_lo", "rmsea_hi", "status")]
#' @export
sieve_optimize_items <- function(items_df, meta, max_drop = 1L, min_keep = 3L,
                                 cap = 300L, simple_structure = TRUE,
                                 progress = NULL, min_facet = NULL,
                                 must_keep = NULL, undersized = "hold",
                                 seed = NULL, project = NULL) {
  if (!is.null(project)) sieve_require_optimization(project)
  started <- proc.time()[["elapsed"]]
  refusal <- function(why, total = 0, plan = NULL, ...) {
    c(list(
      error = why, total = total, evaluated = 0L, complete = FALSE,
      constraints = plan$constraints, constructs = plan$constructs,
      facets = plan$facets
    ), list(...))
  }
  con <- sieve_capture(sieve_optimizer_constraints(
    max_drop = max_drop, min_keep = min_keep, min_facet = min_facet,
    must_keep = must_keep, undersized = undersized
  ))
  if (!con$ok) {
    return(refusal(paste0("The search was not run. ", con$error)))
  }
  if (!is.numeric(cap) || length(cap) != 1L || !is.finite(cap) || cap < 1) {
    return(refusal("The search was not run. The evaluation cap must be a single number, one or more."))
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed))) {
    return(refusal("The search was not run. The seed must be a single whole number, or empty."))
  }
  plan <- sieve_optimizer_plan(meta, con$value)
  if (!is.null(plan$error)) {
    return(refusal(paste0("The search was not run. ", plan$error), plan = plan))
  }

  # The capacity limit, applied to the algebraic count before any candidate
  # exists. The counts for smaller settings are read from the same plan, so
  # a refusal costs the same whatever maximum to drop was asked for.
  if (plan$total > cap) {
    alt <- sieve_plan_alternatives(plan)
    fits <- alt[alt$total <= cap, , drop = FALSE]
    hint <- if (nrow(fits)) {
      best <- fits[nrow(fits), ]
      sprintf(
        " Dropping at most %d per construct would give %s candidate%s, within the cap.",
        best$max_drop,
        sieve_count_text(best$total, best$total * length(plan$active) < 2^53),
        if (best$total == 1) "" else "s"
      )
    } else {
      ""
    }
    how <- if (plan$exact) {
      "The count is exact and was computed without listing the candidates."
    } else if (is.finite(plan$total)) {
      paste0(
        "The count was computed without listing the candidates; a number this large is ",
        "stated approximately, not to the last digit."
      )
    } else {
      paste0(
        "The count was computed without listing the candidates and is larger than the ",
        "largest number that can be represented."
      )
    }
    return(refusal(
      sprintf(
        paste0(
          "The search was not run, and no candidate was built or fit. These settings ",
          "define %s candidate item sets, above the cap of %s. %s%s Lower the maximum to ",
          "drop, raise a minimum, add must-keep items, or raise the cap."
        ),
        sieve_count_text(plan$total, plan$exact), sieve_count_text(cap), how, hint
      ),
      total = plan$total, plan = plan, exact = plan$exact, alternatives = alt
    ))
  }
  absent <- setdiff(plan$active, names(items_df))
  if (length(absent)) {
    return(refusal(
      sprintf(
        "The search was not run. The data do not contain the included item%s %s.",
        if (length(absent) == 1L) "" else "s", paste(absent, collapse = ", ")
      ),
      total = plan$total, plan = plan
    ))
  }

  kept_list <- sieve_enumerate_candidates(plan)
  n_cand <- length(kept_list)

  # Assert the contract on what was built, independently of how it was built.
  checks <- lapply(kept_list, sieve_check_candidate, meta = meta, constraints = con$value)
  failed <- which(!vapply(checks, function(ch) ch$ok, logical(1)))
  if (n_cand != plan$total || !identical(kept_list[[1]], plan$active) ||
    anyDuplicated(vapply(kept_list, paste, character(1), collapse = "\r")) ||
    length(failed)) {
    return(refusal(
      paste0(
        "The search was not run. The candidates that were built do not satisfy the ",
        "constraints they were built from. This is an error in SIEVER, not in your settings; ",
        "please report it.",
        if (length(failed)) paste0(" ", checks[[failed[1]]]$violations[1]) else ""
      ),
      total = plan$total, plan = plan
    ))
  }

  cons <- plan$constructs$construct
  rows <- vector("list", n_cand)
  warn <- vector("list", n_cand)
  omega <- matrix(NA_real_, n_cand, length(cons), dimnames = list(NULL, cons))
  omega_n <- matrix(NA_integer_, n_cand, length(cons), dimnames = list(NULL, cons))
  omega_state <- matrix(NA_character_, n_cand, length(cons), dimnames = list(NULL, cons))
  reference_estimator <- NULL
  # The families of fit indices of every candidate, kept so that one family
  # can be chosen for the whole table once all candidates are fit.
  measured <- vector("list", n_cand)
  rng <- if (isTRUE(simple_structure)) sieve_draw_seed(seed) else NULL
  for (i in seq_len(n_cand)) {
    sc <- sieve_score_candidate(items_df, meta, kept_list[[i]],
      simple_structure = simple_structure,
      reference_estimator = reference_estimator,
      seed = rng$seed
    )
    if (i == 1L) {
      reference_estimator <- sc$estimator
    }
    rows[[i]] <- cbind(
      data.frame(
        id = i, role = if (i == 1L) "baseline" else "candidate",
        stringsAsFactors = FALSE
      ),
      sc$row
    )
    omega[i, ] <- sc$omega[cons]
    omega_n[i, ] <- sc$omega_n[cons]
    omega_state[i, ] <- sc$omega_state[cons]
    warn[[i]] <- sc$warnings
    if (!is.null(sc$families)) {
      measured[[i]] <- sc$families
    }
    if (!is.null(progress)) {
      progress(i)
    }
  }
  table <- do.call(rbind, rows)
  rownames(table) <- NULL

  # CFI and RMSEA are in ONE family for every candidate. The candidates are
  # set side by side on these columns, and the families are different
  # statistics: a pool with ordered and continuous items has robust indices
  # only for the candidates that drop every continuous item, so reading each
  # candidate in its own first available family would put robust values
  # beside scaled ones. A candidate whose model has usable indices but no
  # value in the shared family is listed with that reason and is not ranked
  # on fit.
  shared <- sieve_fit_common(measured)
  in_family <- c("cfi", "rmsea", "rmsea_lo", "rmsea_hi")
  had_fit <- is.finite(table$cfi) | is.finite(table$rmsea)
  for (index in in_family) {
    table[[index]] <- ifelse(had_fit, shared$values[[index]], NA_real_)
  }
  lost <- had_fit & !(is.finite(table$cfi) & is.finite(table$rmsea))
  if (any(lost)) {
    why_lost <- if (is.na(shared$family)) {
      "fit not ranked (no one family of fit indices is reported for every item set)"
    } else {
      sprintf("fit not ranked (lavaan reported no %s CFI or RMSEA for this model)", shared$family)
    }
    join <- function(a, b) ifelse(nzchar(a), paste(a, b, sep = "; "), b)
    table$fit_note[lost] <- join(table$fit_note[lost], why_lost)
    table$reason[lost] <- join(table$reason[lost], why_lost)
    table$fit_ok[lost] <- FALSE
    wanted <- cbind(
      table$reliability_ok, table$fit_ok,
      if (isTRUE(simple_structure)) table$structure_ok
    )
    table$status <- ifelse(rowSums(wanted) == ncol(wanted), "eligible",
      ifelse(rowSums(wanted) > 0, "partly eligible", "not eligible")
    )
  }

  held <- sieve_held_text(plan$constructs, plan$facets, link = ": ")
  count_note <- function(ok, what) {
    if (all(ok)) {
      NULL
    } else {
      sprintf(
        "%s for %d of %d candidates; the others are listed with the reason and are not ranked on it.",
        what, sum(ok), length(ok)
      )
    }
  }
  improper <- sum(table$reliability_code == "improper")
  notes <- c(
    if (length(held)) {
      paste(held, "All of its items are in every candidate.")
    },
    count_note(table$reliability_ok, "The omega summaries can be ordered"),
    if (improper) {
      sprintf(paste0(
        "The omega of %d of %d candidates comes from an improper one-factor ",
        "solution (a negative residual variance estimate) or lies outside 0 to 1; ",
        "it is shown, marked, and not ranked."
      ), improper, n_cand)
    },
    count_note(table$fit_ok, "The fit indices can be ordered"),
    if (isTRUE(simple_structure)) {
      count_note(table$structure_ok, "The simple structure summaries are available")
    }
  )

  list(
    table = table,
    kept = kept_list,
    total = plan$total,
    evaluated = n_cand,
    complete = TRUE,
    constraints = con$value,
    constructs = plan$constructs,
    facets = plan$facets,
    omega = omega,
    omega_n = omega_n,
    omega_state = omega_state,
    warnings = warn,
    simple_structure = isTRUE(simple_structure),
    rng = rng,
    fit_family = shared$family,
    fit_family_reason = shared$reason,
    fit_conf_level = shared$conf_level,
    elapsed = proc.time()[["elapsed"]] - started,
    notes = if (length(notes)) notes else character(0)
  )
}

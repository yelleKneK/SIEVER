# fct_diagnostics.R
# The analytic core of the winnowing engine: live per item diagnostics over
# the currently active item pool. Computations are point estimates so the
# refit stays near immediate on every park or restore; the authoritative
# omega with a confidence interval is computed deliberately in the
# Reliability module, not in this live loop.

#' Align extracted factors to presumed constructs
#'
#' Greedy one to one assignment of each empirical factor to the construct
#' whose items load on it most strongly (in mean absolute loading). Used to
#' decide, for each item, which factor is its target and which are off
#' target, so a salient off target loading can be flagged as a reassignment
#' prompt rather than a deletion signal.
#'
#' @param P A loadings matrix (items by factors), with item row names.
#' @param item_construct Character vector of the construct for each row of P.
#' @param constructs The unique construct names.
#' @return A named character vector mapping each construct to a factor name.
#' @author Ken Kelley
#' @noRd
sieve_align_factors <- function(P, item_construct, constructs) {
  facs <- colnames(P)
  score <- matrix(
    0, length(constructs), length(facs),
    dimnames = list(constructs, facs)
  )
  for (ci in seq_along(constructs)) {
    rows <- which(item_construct == constructs[ci])
    if (length(rows)) {
      score[ci, ] <- colMeans(abs(P[rows, , drop = FALSE]))
    }
  }
  assignment <- stats::setNames(rep(NA_character_, length(constructs)), constructs)
  avail <- facs
  ord <- order(apply(score, 1, max), decreasing = TRUE)
  for (ci in ord) {
    cn <- constructs[ci]
    if (!length(avail)) break
    best <- avail[which.max(score[cn, avail, drop = TRUE])]
    assignment[cn] <- best
    avail <- setdiff(avail, best)
  }
  assignment
}
#' Robust omega point estimate for a set of items
#'
#' The Items tab's reliability while items are winnowed: robust omega, the
#' squared sum of the one-factor loadings over the variance of the sum from
#' the unrestricted (observed) covariance matrix (Kelley & Pornprasertmanit,
#' 2016), a point estimate with no interval and no bootstrap per click. For
#' continuous items it is `DMAR::reliability_omega(denominator = "observed")`
#' (on the covariance matrix of the complete cases, `S` and `N`, which gives
#' the estimate of its raw-data path without the MLR statistic), for ordered
#' items `DMAR::reliability_omega_categorical(ci_method = "none")`, whose
#' observed denominator is built from the unrestricted polychoric correlations
#' (`denominator = "observed"` is passed explicitly on both routes).
#' "Robust" means robust to misspecification of the total variance only: the
#' numerator is model based, so a doublet among a few same-signed items
#' raises it under either denominator.
#'
#' With `missing = "fiml"` the continuous items are fit by full
#' information maximum likelihood on every row with an observed item (or
#' auxiliary), `DMAR::reliability_omega(missing = "fiml")`, whose observed
#' denominator is the saturated FIML covariance matrix: the Reliability
#' tab's treatment (`sieve_reliability_spec(missing = "fiml")`).
#'
#' @param X A data frame of item columns (complete cases when `missing` is
#'   `"listwise"`), and the auxiliary columns named in `aux`.
#' @param ordered `TRUE` to treat the items as ordered categories.
#' @param missing `"listwise"` (complete cases, the covariance matrix and N)
#'   or `"fiml"`.
#' @param aux Continuous auxiliary columns of `X` for FIML.
#' @return A single numeric estimate, or `NA`, with the attribute
#'   `residual_cor` when DMAR supplies the single-factor residual
#'   correlations (`list(matrix, largest, items)`) and the attribute
#'   `messages` when the engine noted something about the coefficient
#'   (DMAR's note that the codes of ordered items are not one rescaling of
#'   their ranks, for example), captured rather than suppressed so the
#'   caller can show it.
#' @author Ken Kelley
#' @noRd
sieve_item_omega_point <- function(X, ordered = FALSE, missing = "listwise", aux = NULL) {
  if (length(setdiff(names(X), aux)) < 3L) {
    return(NA_real_)
  }
  heard <- new.env(parent = emptyenv())
  heard$messages <- character(0)
  ro <- tryCatch(
    suppressWarnings(withCallingHandlers(
      sieve_one_factor(X, ordered = ordered, missing = missing, aux = if (length(aux)) aux, fast = TRUE),
      message = function(m) {
        heard$messages <- c(heard$messages, sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )),
    error = function(e) NULL
  )
  if (is.null(ro)) {
    return(NA_real_)
  }
  v <- ro$value[ro$term == "estimate"]
  out <- if (length(v)) v[1] else NA_real_
  rc <- attr(ro, "residual_cor")
  if (!is.null(rc)) attr(out, "residual_cor") <- rc
  if (length(heard$messages)) attr(out, "messages") <- unique(heard$messages)
  out
}

#' The note beside a construct's live omega on the Items tab
#'
#' For a construct of ordered items the coefficient is categorical omega,
#' the reliability of the sum of category ranks; when the codes do not step
#' by one shared amount, SIEVER's coding note (`sieve_reliability_coding()`,
#' without the coded composite's reliability, which the Reliability tab
#' computes) replaces DMAR's message on the codes. Every other message the
#' engine gave is kept, as given.
#'
#' @param X The construct's complete rows.
#' @param items The construct's items.
#' @param meta The item ledger.
#' @param ordered Whether the items are read as ordered.
#' @param messages The engine's messages (`attr(, "messages")` of
#'   `sieve_item_omega_point()`).
#' @return `NA` or one string.
#' @author Ken Kelley
#' @noRd
sieve_item_omega_note <- function(X, items, meta, ordered, messages = NULL) {
  coding <- if (isTRUE(ordered) && length(items) >= 3L) {
    spec <- list(
      items = items, coefficient = "omega_categorical", item_class = "categorical",
      denominator = "observed", ci_target = "none"
    )
    tryCatch(sieve_reliability_coding(X, spec, meta = meta, estimate = FALSE), error = function(e) NULL)
  }
  messages <- unique(gsub("\\s+", " ", as.character(messages %||% character(0))))
  note <- coding$note %||% NA_character_
  if (!is.na(note)) {
    messages <- messages[!grepl("^Categorical omega scores the categories of each item as consecutive ranks", messages)]
  }
  text <- c(
    if (!is.na(note)) note,
    if (length(messages)) sprintf("The engine noted: %s.", paste(sprintf("\"%s\"", messages), collapse = "; "))
  )
  if (length(text)) paste(text, collapse = " ") else NA_character_
}

#' The largest single-factor residual correlation among a construct's items
#'
#' Beside robust omega the Items tab shows the largest residual correlation of
#' the one-factor model of the construct's items and its item pair: a rising
#' omega while a large residual pair is retained signals redundancy, not
#' better measurement. A small largest residual does not certify
#' unidimensionality: when shared variance sits on the dominant items, the
#' single factor absorbs it and the misfit appears among the other items. DMAR
#' returns the residuals as `attr(x, "residual_cor")`, passed here as
#' `supplied`; when that attribute is absent (the omega call failed, or
#' DMAR could not form the residuals), they are computed here, as DMAR
#' computes them: the correlation residuals (as
#' `lavaan::lavResiduals(type = "cor")` defines them, observed minus model
#' implied on the correlation scale) of the one-factor model with the factor
#' variance fixed at one, fit by ML to continuous items and by WLSMV to
#' ordered items (on the polychoric correlations).
#'
#' @param X A data frame of item columns (complete cases when `missing` is
#'   `"listwise"`), and the auxiliary columns named in `aux`.
#' @param ordered `TRUE` to treat the items as ordered categories.
#' @param supplied The `residual_cor` attribute DMAR returned, or `NULL`.
#' @param missing `"listwise"` or `"fiml"` (the observed matrix is then the
#'   saturated FIML estimate).
#' @param aux Continuous auxiliary columns of `X` for FIML.
#' @return A list with `value` (the signed residual correlation of largest
#'   absolute value, or `NA`), `items` (its two items), `matrix`, and
#'   `reason` (why there is no value, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_single_factor_residual <- function(X, ordered = FALSE, supplied = NULL, missing = "listwise", aux = NULL) {
  none <- function(reason) list(value = NA_real_, items = character(0), matrix = NULL, reason = reason)
  aux <- intersect(as.character(aux), names(X))
  if (length(setdiff(names(X), aux)) < 4L) {
    return(none("Residual diagnostics are not interpreted with fewer than four items; the usual three-item model has zero degrees of freedom."))
  }
  R <- NULL
  if (is.list(supplied) && is.matrix(supplied$matrix)) {
    R <- supplied$matrix
  } else {

    nm <- setdiff(names(X), aux)
    d <- X[, c(nm, aux), drop = FALSE]
    names(d) <- paste0("v", seq_len(ncol(d)))
    renamed_aux <- if (length(aux)) names(d)[length(nm) + seq_along(aux)]
    fit <- tryCatch(suppressWarnings(sieve_one_factor(d, ordered = ordered,
      missing = missing, aux = renamed_aux, output = "fit")), error = function(e) NULL)
    if (is.null(fit) || !isTRUE(lavaan::lavInspect(fit, "converged"))) {
      return(none("The one-factor model did not converge."))
    }
    # The correlation residuals as lavaan::lavResiduals(type = "cor")
    # defines them (observed minus model implied, each on the correlation
    # scale), computed from the two matrices directly: lavResiduals() also
    # forms their standard errors from the asymptotic covariance of the
    # sample statistics, a matrix with as many rows as there are moments,
    # which takes hours for 68 items.
    R <- tryCatch({
      observed <- lavaan::lavInspect(fit, "sampstat")$cov
      implied <- lavaan::lavInspect(fit, "implied")$cov
      unclass(stats::cov2cor(unclass(observed))) - unclass(stats::cov2cor(unclass(implied)))
    }, error = function(e) NULL)
    if (is.null(R)) {
      return(none("The residual correlations could not be computed."))
    }
    inner <- names(d)[seq_along(nm)]
    R <- R[inner, inner, drop = FALSE]
    dimnames(R) <- list(nm, nm)
  }
  off <- R
  off[!upper.tri(off)] <- NA
  if (!any(is.finite(off))) {
    return(none("No residual correlation could be computed."))
  }
  at <- which(abs(off) == max(abs(off), na.rm = TRUE), arr.ind = TRUE)[1L, ]
  list(
    value = unname(off[at[1L], at[2L]]), items = c(rownames(R)[at[1L]], colnames(R)[at[2L]]),
    matrix = R, reason = NULL
  )
}

#' Cronbach's alpha point estimate for a set of items
#'
#' A fast point estimate via DMAR::reliability_alpha with no confidence
#' interval, used as the legacy alternative to omega inside the live winnowing
#' loop (the authoritative alpha with an interval is offered in the Reliability
#' module). Returns NA with fewer than two items or when the estimate cannot be
#' computed. Uses DMAR rather than a bespoke formula so the same engine backs
#' alpha throughout (see `sieve_item_omega_point()`).
#'
#' DMAR's analytic estimator, as the Reliability tab computes alpha
#' (`sieve_reliability_alpha_plan()`), under the same missing data
#' treatment as the construct's omega.
#'
#' @param X A data frame of item columns, and the auxiliary columns named
#'   in `aux`.
#' @param missing `"listwise"` or `"fiml"`.
#' @param aux Continuous auxiliary columns of `X` for FIML.
#' @return A single numeric alpha, or NA, with the attribute `messages`
#'   when the engine noted something about it (captured, not suppressed).
#' @author Ken Kelley
#' @noRd
sieve_alpha_point <- function(X, missing = "listwise", aux = NULL) {
  if (length(setdiff(names(X), aux)) < 2L) {
    return(NA_real_)
  }
  heard <- new.env(parent = emptyenv())
  heard$messages <- character(0)
  args <- list(data = X, estimator = "analytic", missing = missing, ci_method = "none")
  if (length(aux)) args$aux <- aux
  ra <- tryCatch(
    suppressWarnings(withCallingHandlers(
      do.call(DMAR::reliability_alpha, args),
      message = function(m) {
        heard$messages <- c(heard$messages, sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )),
    error = function(e) NULL
  )
  if (is.null(ra)) {
    return(NA_real_)
  }
  v <- ra$value[ra$term == "estimate"]
  out <- if (length(v)) v[1] else NA_real_
  if (length(heard$messages)) attr(out, "messages") <- unique(heard$messages)
  out
}

#' The orientation of a set of loadings
#'
#' The sign of a factor is arbitrary (lavaan and DMAR orient it by the
#' first item), so "opposite in sign" is read against the orientation most
#' of the items share: the sign of the majority of the finite nonzero
#' loadings, and with as many of each sign, the sign of their sum.
#'
#' @param loadings Numeric loadings.
#' @return `1`, `-1`, or `0` when no orientation can be read.
#' @author Ken Kelley
#' @noRd
sieve_keying_orientation <- function(loadings) {
  loadings <- loadings[is.finite(loadings) & loadings != 0]
  if (length(loadings) < 2L) return(0)
  balance <- sum(sign(loadings))
  if (balance != 0) return(sign(balance))
  sign(sum(loadings))
}

#' Items whose target loading is opposite in sign to the others of their factor
#'
#' The Items tab's keying check. Within each target factor (the construct,
#' or the facet of the one construct), an item is flagged when its target
#' loading is opposite in sign to the orientation the other items share
#' (`sieve_keying_orientation()`) and salient in size (at least `salient`,
#' the cross loading threshold), so a near-zero loading of either sign is
#' left to the low loading flag. Such an item subtracts from the common
#' part of the total as scored: robust omega, alpha, every item-rest
#' correlation of its construct and the change if dropped describe that
#' total, while a reverse keyed item that is reverse scored would add to
#' it. The note says so, and that the action is to reverse score it, not to
#' exclude it; for an item the ledger already marks as reverse keyed, it
#' says that the values may have been reversed twice. It also says that
#' the item-rest correlations and changes if dropped of every item of the
#' construct read that total and are not counted as reasons to exclude
#' any of them (`sieve_item_live_flags()`).
#'
#' @param items The per item data frame of `sieve_item_diagnostics()`
#'   (`item`, `construct`, `factor_group`, `loading_target`).
#' @param salient The salient loading threshold.
#' @param meta Optional item ledger with `reverse`.
#' @return A list with `flag` (a logical per row of `items`) and `notes`
#'   (a character vector named by construct, `NA` where no item is flagged).
#' @author Ken Kelley
#' @noRd
sieve_item_keying <- function(items, salient = 0.30, meta = NULL) {
  n <- NROW(items)
  flag <- rep(FALSE, n)
  constructs <- unique(as.character(items$construct))
  notes <- stats::setNames(rep(NA_character_, length(constructs)), constructs)
  if (!n) return(list(flag = flag, notes = notes))
  loading <- as.numeric(items$loading_target)
  group <- as.character(items$factor_group %||% items$construct)
  salient <- sieve_finite_or(as.numeric(salient), 0.30)
  orient <- list()
  for (g in unique(group[!is.na(group)])) {
    at <- which(group %in% g & is.finite(loading))
    s <- sieve_keying_orientation(loading[at])
    orient[[g]] <- s
    if (s == 0) next
    flag[at] <- sign(loading[at]) == -s & abs(loading[at]) >= salient
  }
  reverse <- if (!is.null(meta) && "reverse" %in% names(meta)) {
    meta$reverse[match(items$item, meta$item)] %in% TRUE
  } else {
    rep(FALSE, n)
  }
  for (cn in constructs) {
    at <- which(flag & items$construct %in% cn)
    if (!length(at)) next
    text <- vapply(at, function(i) {
      g <- group[i]
      others <- sum(group %in% g & is.finite(loading) & sign(loading) == orient[[g]])
      where <- if (identical(g, cn)) {
        sprintf("the other %d item%s of %s", others, if (others == 1L) "" else "s", cn)
      } else {
        sprintf("the other %d item%s of its factor (%s, within %s)", others, if (others == 1L) "" else "s", g, cn)
      }
      if (reverse[i]) {
        sprintf(paste0(
          "%s is marked reverse keyed and was reverse scored, yet its loading, %.3f, is opposite in sign to %s: ",
          "if the file already held it reverse scored, its values have been reversed twice; record it on the ",
          "Ingest tab as already reverse scored rather than excluding it."
        ), items$item[i], loading[i], where)
      } else {
        sprintf(paste0(
          "%s loads %.3f, opposite in sign to %s. Robust omega, alpha, the item-rest correlations and the ",
          "change if dropped describe the total as scored, in which %s subtracts from the common part, so omega ",
          "rises without it for that reason. If %s is reverse keyed, set its Reverse? cell to Yes on the Ingest ",
          "tab rather than excluding it."
        ), items$item[i], loading[i], where, items$item[i], items$item[i])
      }
    }, character(1))
    notes[[cn]] <- paste(c(text, sprintf(paste0(
      "Until the keying is resolved, the item-rest correlations and the changes if dropped of all the items ",
      "of %s read that total, so none of them is counted as a reason to exclude an item."
    ), cn)), collapse = " ")
  }
  list(flag = flag, notes = notes)
}

#' The Items tab's review flags at the page's thresholds
#'
#' One reading of the per item diagnostics, shared by the grid's
#' Diagnostic flags column and the review brief
#' (`sieve_construct_brief()`), so the two never disagree. Thresholds only
#' re-read the stored statistics (no refit). Low loading is judged by the
#' size of the target loading, so a strongly reverse keyed item is not low
#' loading; an item whose salient target loading is opposite in sign to
#' the other items of its factor is flagged for its keying
#' (`sieve_item_keying()`). In a construct that holds such an item, every
#' item-rest correlation and every change if dropped reads a total in
#' which that item subtracts from the common part, so the item-rest and
#' "rises if dropped" signals of all of the construct's items are withheld
#' (`withheld`) until the keying is resolved: they would point at the
#' correctly keyed items as well as at the opposite one.
#'
#' @param items The per item data frame of `sieve_item_diagnostics()`.
#' @param thresholds Named cutoffs (`sieve_default_thresholds()`).
#' @param meta Optional item ledger with `reverse`.
#' @param use_alpha Whether "rises if dropped" reads alpha rather than
#'   robust omega.
#' @return A data frame, one row per row of `items`, of logical columns
#'   `keying`, `load`, `comm`, `itc`, `relup`, `dup`, `cross` and
#'   `withheld`.
#' @author Ken Kelley
#' @noRd
sieve_item_live_flags <- function(items, thresholds = sieve_default_thresholds(), meta = NULL, use_alpha = FALSE) {
  th <- utils::modifyList(sieve_default_thresholds(), as.list(thresholds %||% list()))
  n <- NROW(items)
  if (!n) {
    return(data.frame(keying = logical(0), load = logical(0), comm = logical(0), itc = logical(0),
      relup = logical(0), dup = logical(0), cross = logical(0), withheld = logical(0)))
  }
  num <- function(col) if (col %in% names(items)) as.numeric(items[[col]]) else rep(NA_real_, n)
  loading <- num("loading_target")
  offmax <- num("loading_off_max")
  reldrop <- if (isTRUE(use_alpha)) num("alpha_drop_delta") else num("omega_drop_delta")
  keying <- sieve_item_keying(items, salient = th$salient %||% 0.30, meta = meta)$flag
  construct <- as.character(items$construct)
  withheld <- construct %in% unique(construct[keying])
  data.frame(
    keying = keying,
    load = (abs(loading) < th$load_low) %in% TRUE,
    comm = (num("communality") < th$communality_low) %in% TRUE,
    itc = (num("item_total") < th$item_total_low) %in% TRUE & !withheld,
    relup = (reldrop > 0) %in% TRUE & !withheld,
    dup = (num("redundancy_max") > th$redundancy_high) %in% TRUE,
    cross = (offmax >= th$salient & offmax >= (abs(loading) - 0.10)) %in% TRUE,
    withheld = withheld
  )
}

#' Live per Item Diagnostics for the Winnowing Engine
#'
#' Fits a single factor analysis over the active item pool (theory aligned,
#' k factors at the number of constructs, or at the number of facets when
#' one construct is split into two or more facets; `DMAR::fa_exploratory()`,
#' maximum likelihood on the correlation matrix, with direct oblimin when
#' k > 1) and computes, for each active item, three kinds of diagnostics:
#' statistical removal signals (low standardized loading, low communality,
#' low corrected item total correlation, an omega that rises when the item
#' is dropped, near duplicate redundancy), a distinct cross loading
#' reassignment prompt (a salient loading on a non target factor), and a
#' keying check (a salient target loading opposite in sign to the other
#' items of its factor; see below).
#'
#' All quantities are point estimates so the refit stays responsive. The
#' function never errors on a difficult pool: it returns what it can and
#' records the rest in `notes`.
#'
#' For a one-construct study whose included items are split into two or more
#' facets, the factors are the facets (the grouping of
#' `sieve_structure_groups()`): each item's target is its facet's factor,
#' `off_construct` names the facet of the strongest other factor, and an item
#' of the construct without a facet has no target. `factor_level` says which
#' (`"construct"` or `"facet"`).
#'
#' With a single construct and no facets the factor analysis has one factor,
#' so there is no factor other than the target for an item to load on. The off target
#' loading (`loading_off_max`) is then `NA`, not zero (zero would read as an
#' estimate), `off_construct` and `suggest_construct` are `NA`, and
#' `cross_loading` is `FALSE`.
#'
#' The correlation matrix the factor analysis and the redundancy flag read
#' is the shared basis of the included items when one is supplied (see
#' `sieve_state_basis()`: Pearson for continuous items, polychoric or
#' tetrachoric for ordered or binary items, the same matrix the
#' Dimensionality and EFA tabs use), and the Pearson matrix of the complete
#' cases otherwise. The per construct reliability coefficients and the
#' corrected item total correlations are computed from the item responses
#' either way.
#'
#' @details
#' Supply numeric values after intended reverse scoring. The ledger needs
#' `item`, `label`, `construct`, `status`, and `type`; only active items enter
#' the pool. These exploratory point estimates and review flags do not establish
#' item quality or construct validity and supply no design-adjusted inference.
#' Numerical results retain their precision. Use [sieve_retention_cor()] on
#' complete active-item responses to supply the same categorical correlation
#' basis used by the interface; without `basis`, the fallback is Pearson.
#'
#' Each construct's omega, alpha, omega and alpha if each item were
#' dropped, and largest residual correlation are computed on that
#' construct's own rows under the Reliability tab's missing data treatment
#' (`missing`): by full information maximum likelihood for continuous items
#' (every row with an observed item of the construct, or an auxiliary),
#' and on the rows complete on the construct's own items for ordered items,
#' whose polychoric correlations need complete rows, or with `missing =
#' "listwise"`. No other construct's items select those rows, so a
#' construct's values do not change when items of another construct are
#' included or excluded, and a planned-missing design, in which no row is
#' complete on every item, still gives each construct its coefficients.
#' The exploratory factor analysis reads the shared correlation matrix of
#' all included items (`basis`): under FIML the saturated FIML matrix of
#' every row, otherwise the rows complete on every included item
#' (`n_obs`). When too few rows are complete for that matrix and `basis`
#' is not the saturated FIML matrix (for continuous items,
#' `sieve_retention_cor(data, missing = "fiml")`), the factor analysis is
#' not fit (`reliability_only` is `TRUE`, `status$code` is `"not_fit"`,
#' and `notes` says what it needs): the loading, communality, complexity,
#' cross loading and redundancy columns are `NA`, and the reliability
#' columns are given.
#'
#' An item whose salient target loading is opposite in sign to the other
#' items of its factor (`flag_keying`) is judged
#' low loading by the size of its loading, as every other item is. Every
#' item-rest correlation and change if dropped of its construct reads a
#' total in which it subtracts from the common part: its own negative
#' item-rest correlation and the rise in omega without it, and the lowered
#' item-rest correlations and rises of the correctly keyed items beside
#' it. So for every item of that construct (`keying_withheld`) the
#' item-rest and omega-if-dropped flags are not counted among the removal
#' signals, and the construct's `keying_note` says that reverse scoring,
#' not exclusion, is the action when the item is reverse keyed.
#' @param data A numeric data frame holding all item columns.
#' @param meta Item ledger with the fields described in Details.
#' @param thresholds Named flag cutoffs: `load_low` (default .40), `salient`
#'   (.30), `communality_low` (.30), `item_total_low` (.30), and
#'   `redundancy_high` (.80). These are review conventions, not validity tests.
#' The per construct block (the corrected item total correlations, the
#' construct's omega and alpha, and the coefficient if each item were
#' dropped, which refits the one-factor model once per item) is the slow
#' part: one refit per included item. With `keep`, an environment the
#' caller owns, each construct's block is kept under the identity of its
#' items and their responses (see `sieve_keep()`), so an edit that moves
#' or removes one item recomputes the constructs it touched and no other.
#' With `cores` above one the refits run on bounded socket workers;
#' they are deterministic, so the result does not
#' depend on the workers.
#'
#' @param basis Optional: the shared correlation basis from
#'   `sieve_state_basis()`. It is used when it holds every active item and
#'   was computed on the same complete cases; otherwise a Pearson matrix is
#'   computed here and `notes` says so.
#' @param keep Optional environment in which the per construct blocks are
#'   kept between calls.
#' @param cores Workers for the per item reliability refits.
#' @param ordered The items whose reliability is computed as ordered
#'   categories (categorical robust omega on the polychoric correlations);
#'   by default the items typed ordered or binary. A construct is read as
#'   ordered when every one of its items is.
#' @param rng Optional seed record from `sieve_draw_seed()` for the factor
#'   analysis. `DMAR::fa_exploratory()` starts an oblique rotation from 30
#'   random positions, so without a seed the loadings depend on the caller's
#'   random number stream (in their last digits, and possibly in the order
#'   of the factors). With a seed DMAR rotates on a local stream under the
#'   generator `rng$kind` names, the caller's stream is restored, and the
#'   same data and seed give the same loadings.
#' @param solution Optional: the same exploratory model fit elsewhere (the
#'   EFA tab's kept `DMAR::fa_exploratory()` solution, see
#'   `sieve_efa_store_match()`), a list with `fit` (a
#'   `sieve_efa_dmar_solution()`), `status`, `rng` (the seed record it was
#'   fit under), `warnings` and `k`. It is read in place of fitting when its
#'   `k` is the number of factors here, with its `rng` reported; the caller
#'   checks that the items, cases, correlations, estimates and rotation are
#'   the same.
#' @param efa_record Optional: the record of this very DMAR call
#'   (`sieve_efa_dmar_run()` of `sieve_efa_point_call()` with the same
#'   matrix, k and seed) computed elsewhere, as the Items tab's background R
#'   process computes it when the factor analysis is estimated above two
#'   seconds; it is read in place of fitting.
#' @param missing The missing data treatment of each construct's
#'   reliability: `"fiml"` (the default, DMAR's and the Reliability tab's:
#'   full information maximum likelihood for continuous items, on every row
#'   with an observed item of the construct) or `"listwise"` (the rows
#'   complete on the construct's own items). Ordered items use the rows
#'   complete on their construct's items either way.
#' @param aux Optional names of continuous auxiliary columns of `data` that
#'   enter each continuous construct's FIML fit (the study's declared
#'   auxiliaries, as on the Reliability tab); ignored under `"listwise"`.
#' @return A list with `items` (a per item diagnostics data frame, with
#'   `flag_keying` beside the other flags, each flag saying whether the
#'   item trips its cutoff, and `keying_withheld`, `TRUE` for every item of
#'   a construct that holds an item flagged for its keying, whose
#'   item-rest and omega-if-dropped flags `removal_signals` does not
#'   count),
#'   `constructs` (a per construct data frame: robust omega with its label
#'   and, in `omega_matrix`, the matrix of its denominator (the unrestricted
#'   polychoric correlations of ordered items; the saturated FIML covariance
#'   matrix of a construct fit by FIML, with its auxiliaries; the Pearson
#'   covariances of the item scores of the complete rows otherwise),
#'   `missing_treatment` (`"fiml"` or
#'   `"listwise"`, the treatment used), `n_reliability` (the rows omega and
#'   alpha read: under FIML every row with an observed item of the
#'   construct or an auxiliary) and `n_complete` (the rows complete on the
#'   construct's items), `keying_note` (`NA`, or the sentence naming items
#'   whose salient target loading is opposite in sign to the others of
#'   their factor), `omega_note` (the coding note of ordered items and
#'   whatever else the engine noted, or `NA`), alpha with `alpha_note`, and
#'   the largest single-factor residual correlation with its item pair),
#'   `n_obs` (the listwise
#'   complete sample size used), `k`, `factor_level` and `factor_groups`
#'   (the number of factors, whether they are the constructs or the facets
#'   of the one construct, and their names), `basis_used` (the correlations the factor
#'   analysis read: `"pearson"`, `"polychoric"`, `"tetrachoric"`, or
#'   `"mixed"`), `basis_missing` (`"fiml"` when the matrix is the saturated
#'   FIML matrix), `recomputed` (the constructs whose block was computed in
#'   this call rather than kept), `rng` (the seed record the factor
#'   analysis ran under, or `NULL`), `solution_reused` (`TRUE` when
#'   `solution` was read in place of fitting), `efa_call` (the DMAR call of
#'   this tab's own fit, `sieve_efa_point_call()`), `efa_seconds` (its
#'   measured time, `NA` when it was not fit here), `status` (the factor
#'   analysis's status from `sieve_efa_dmar_status()`: DMAR refuses an
#'   improper solution, one that did not converge, and a likelihood without
#'   a maximum, by class, with its reason; the loadings are then absent),
#'   `warnings` (what DMAR and lavaan signaled), `reliability_only`
#'   (`TRUE` when the factor analysis was not fit for too few complete
#'   rows without a FIML basis, and only the reliability columns are
#'   given), and `notes` (a character vector); or `NULL` when there are
#'   too few active items to fit.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' diag <- sieve_item_diagnostics(items, meta)
#' diag$constructs
#' head(diag$items)
#' @export
#' @importFrom stats cor complete.cases setNames
sieve_item_diagnostics <- function(data, meta, thresholds = sieve_default_thresholds(),
                                   basis = NULL, keep = NULL, cores = 1L, rng = NULL, ordered = NULL,
                                   solution = NULL, efa_record = NULL, missing = c("fiml", "listwise"),
                                   aux = NULL) {
  missing <- match.arg(missing)
  setup <- sieve_item_efa_setup(data, meta, basis)
  if (is.null(setup)) {
    return(NULL)
  }
  # Too few complete rows for the factor analysis and no FIML basis: the
  # reliability columns are still computed, each construct on its own rows.
  reliability_only <- isTRUE(setup$reliability_only)
  if (!is.null(setup$problem) && !reliability_only) {
    return(list(
      items = NULL, constructs = NULL, n_obs = setup$n_obs, basis_used = NA_character_,
      notes = setup$problem
    ))
  }
  active <- setup$active
  items <- setup$items
  if (is.null(ordered)) {
    ordered <- if ("type" %in% names(active)) active$item[active$type %in% c("ordered", "binary")] else character(0)
  }
  X <- setup$X
  n_obs <- setup$n_obs
  notes <- setup$notes
  constructs <- setup$constructs
  factor_level <- setup$factor_level
  factor_group <- setup$factor_group
  factor_groups <- setup$factor_groups
  k <- setup$k
  R <- setup$R
  basis_used <- setup$basis_used

  # Overall factor analysis at k factors, for loadings, communalities,
  # complexity, and cross loadings: DMAR::fa_exploratory(), maximum
  # likelihood on the correlation matrix (the matrix route, S with its N),
  # direct oblimin (none at one factor), from the seed recorded for the
  # pool (sieve_efa_point_call()).
  P <- NULL
  h2 <- stats::setNames(rep(NA_real_, length(items)), items)
  cmplx <- stats::setNames(rep(NA_real_, length(items)), items)
  align <- NULL
  efa_status <- NULL
  efa_warnings <- character(0)
  efa_seconds <- NA_real_
  efa_call <- if (!reliability_only) sieve_efa_point_call(k, R = R, n = setup$n_fit, seed = rng$seed)
  # The same model fit elsewhere (the EFA tab's kept solution, see
  # sieve_efa_store_match()) is read in place of fitting, with the seed it
  # ran under.
  reused <- !reliability_only && !is.null(solution) && !is.null(solution$fit) &&
    identical(as.integer(solution$k), as.integer(k))
  read <- if (reliability_only) {
    list(ok = FALSE, status = list(code = "not_fit", message = paste(
      "too few rows are complete on every included item, and the basis is not the saturated FIML",
      "correlation matrix of every row (the note below says what it needs)."
    )), warnings = character(0))
  } else if (reused) {
    rng <- solution$rng
    list(
      ok = TRUE, status = solution$status %||% list(code = "ok", message = NULL),
      loadings = solution$fit$loadings, communality = solution$fit$communality,
      complexity = solution$fit$complexity, warnings = solution$warnings %||% character(0)
    )
  } else {
    # A record computed elsewhere for this very call (the module's
    # background R process, mod_winnow_server()) is read in
    # place of fitting here.
    record <- if (!is.null(efa_record)) efa_record else sieve_efa_dmar_run(efa_call, rng_kind = rng$kind)
    efa_seconds <- record$seconds %||% NA_real_
    sieve_efa_point_read(record)
  }
  efa_warnings <- read$warnings
  # The solution's status: DMAR refuses an improper solution (a residual
  # variance below zero, which psych::fa() would hold at its bound of
  # 0.005), one that did not converge, and a likelihood without a maximum,
  # by class, with its reason (sieve_efa_dmar_status()); a returned
  # solution is proper.
  efa_status <- read$status
  if (reliability_only) {
    notes <- c(notes, setup$problem)
  } else if (!isTRUE(read$ok)) {
    notes <- c(notes, paste(
      "The exploratory factor analysis gave no loadings, so the loading, h2, cross loading and complexity",
      "columns are empty; the reliability columns do not depend on it."
    ))
  } else {
    P <- read$loadings
    h2[rownames(P)] <- read$communality[rownames(P)]
    cmplx[rownames(P)] <- read$complexity[rownames(P)]
    align <- sieve_align_factors(P, factor_group, factor_groups)
  }

  # Per construct corrected item total correlations and omega (with the
  # leave one out delta), computed within each construct's active items.
  itc <- stats::setNames(rep(NA_real_, length(items)), items)
  itc_n <- stats::setNames(rep(0L, length(items)), items)
  itc_reason <- stats::setNames(rep("", length(items)), items)
  omega_drop_delta <- stats::setNames(rep(NA_real_, length(items)), items)
  alpha_drop_delta <- stats::setNames(rep(NA_real_, length(items)), items)
  con_rows <- vector("list", length(constructs))
  names(con_rows) <- constructs
  computed <- new.env(parent = emptyenv())
  computed$constructs <- character(0)
  cores <- max(1L, as.integer(sieve_finite_or(cores, 1L)))
  computed$pool <- NULL
  computed$worker_notes <- character(0)
  on.exit(sieve_socket_stop(computed$pool), add = TRUE)
  for (cn in constructs) {
    its <- active$item[active$construct == cn]
    Xc_all <- data[, its, drop = FALSE] # all cases, for the item total
    Xc_all[] <- lapply(Xc_all, sieve_as_number)
    # Robust omega reads the items as ordered when every item of the
    # construct is treated as ordered (categorical omega on the polychoric
    # correlations); a construct that mixes types is read as continuous.
    treat_ordered <- length(its) > 0L && all(its %in% ordered)
    mixed <- !treat_ordered && any(its %in% ordered)
    # The construct's own rows, under the Reliability tab's treatment
    # (sieve_item_reliability_frame()): FIML for continuous items, the rows
    # complete on its own items for ordered items or on request. No other
    # construct's items select its rows, so its values do not move when
    # they are included or excluded.
    frame <- sieve_item_reliability_frame(data, its, ordered = treat_ordered, missing = missing, aux = aux)
    Xc <- frame$X
    engine_missing <- frame$engine_missing
    frame_aux <- frame$aux
    compute_block <- function() {
      computed$constructs <- c(computed$constructs, cn)
      b_itc <- stats::setNames(rep(NA_real_, length(its)), its)
      b_om <- stats::setNames(rep(NA_real_, length(its)), its)
      b_al <- stats::setNames(rep(NA_real_, length(its)), its)
      b_worker_notes <- character(0)
      rest_stats <- sieve_item_rest(Xc_all, its)
      b_itc[rest_stats$item] <- rest_stats$r
      om_full <- sieve_item_omega_point(Xc, ordered = treat_ordered, missing = engine_missing, aux = frame_aux)
      # The same residual correlations as the Reliability tab's
      # sieve_reliability_residuals() (observed minus implied, Bollen's
      # scaling), from the engine's one-factor fit on the construct's own
      # rows, so the diagnostics stay fast; a test checks that the two tabs
      # give the same values.
      resid <- sieve_single_factor_residual(Xc, ordered = treat_ordered, supplied = attr(om_full, "residual_cor"),
        missing = engine_missing, aux = frame_aux)
      # What the engine noted about the construct's coefficient, shown once
      # per construct beside the reliability above the grid: for ordered
      # items, SIEVER's coding note (the codes as administered and as
      # scored, empty interior categories) in place of DMAR's message on
      # the codes, and any other message as given.
      om_note <- sieve_item_omega_note(Xc[, its, drop = FALSE], its, meta, treat_ordered, attr(om_full, "messages"))
      om_full <- as.numeric(om_full)
      al_full <- sieve_alpha_point(Xc, missing = engine_missing, aux = frame_aux)
      al_messages <- attr(al_full, "messages")
      al_full <- as.numeric(al_full)
      if (length(its) >= 4L) {
        # One refit per item: the coefficient of the construct without it.
        if (cores > 1L && is.null(computed$pool)) computed$pool <- tryCatch(
          sieve_socket_pool(min(cores, max(table(active$construct)))),
          error = function(e) list(error = conditionMessage(e))
        )
        dropped <- if (!is.null(computed$pool$error)) rep(
          list(structure(
            paste(
              "Socket workers could not start:",
              computed$pool$error
            ),
            class = c(
              "siever_worker_error",
              "character"
            )
          )),
          length(its)
        ) else
          sieve_socket_map(its, "sieve_diagnostic_drop_point",
            args = list(items = its, data = Xc, omega = om_full, alpha = al_full, ordered = treat_ordered,
              missing = engine_missing, aux = frame_aux),
            task_arg = "item", cores = cores, pool = computed$pool
          )
        failed <- vapply(dropped, inherits, logical(1), "siever_worker_error")
        if (any(failed)) b_worker_notes <- sprintf(
          "Drop-item reliability for '%s' is unavailable for %s. %s", cn,
          paste(its[failed], collapse = ", "), paste(unique(unlist(dropped[failed])), collapse = " ")
        )
        for (k in seq_along(its)) {
          d <- dropped[[k]]
          if (is.numeric(d) && length(d) == 2L) {
            b_om[its[k]] <- d[["omega"]] - om_full
            b_al[its[k]] <- d[["alpha"]] - al_full
          }
        }
      }
      list(
        worker_notes = b_worker_notes, itc = b_itc, itc_n = stats::setNames(rest_stats$n, rest_stats$item),
        itc_reason = stats::setNames(rest_stats$reason, rest_stats$item), omega_drop = b_om, alpha_drop = b_al,
        row = data.frame(
          construct = cn, n_active = length(its), omega = om_full,
          omega_label = "Robust omega (observed denominator)",
          omega_matrix = sieve_item_omega_matrix_text(treat_ordered, mixed, engine_missing, frame_aux),
          treatment = if (treat_ordered) "ordered" else "continuous",
          missing_treatment = frame$missing,
          n_reliability = frame$n,
          n_complete = frame$n_complete,
          omega_note = om_note,
          alpha = al_full,
          alpha_note = if (length(al_messages)) {
            sprintf("The engine noted: %s.", paste(sprintf("\"%s\"", gsub("\\s+", " ", al_messages)), collapse = "; "))
          } else {
            NA_character_
          },
          residual_max = resid$value,
          residual_pair = if (length(resid$items) == 2L) paste(resid$items, collapse = " and ") else NA_character_,
          residual_reason = resid$reason %||% NA_character_,
          stringsAsFactors = FALSE
        )
      )
    }
    block <- if (is.null(keep)) {
      compute_block()
    } else {
      # Kept under the construct's items and their responses: the rows its
      # reliability reads (its own, with any auxiliaries) and all cases (for
      # the item total). Nothing of another construct enters the key.
      sieve_keep(
        keep, paste0("construct:", cn),
        sieve_diagnostics_block_key(its, Xc, Xc_all, treat_ordered, frame$missing),
        compute_block
      )
    }
    computed$worker_notes <- c(computed$worker_notes, block$worker_notes %||% character(0))
    if (!is.null(keep) && length(block$worker_notes)) rm(list = paste0("construct:", cn), envir = keep)
    itc[its] <- block$itc[its]
    itc_n[its] <- block$itc_n[its]
    itc_reason[its] <- block$itc_reason[its]
    omega_drop_delta[its] <- block$omega_drop[its]
    alpha_drop_delta[its] <- block$alpha_drop[its]
    con_rows[[cn]] <- block$row
  }

  # Assemble the per item table with the three flag classes.
  rows <- lapply(seq_len(nrow(active)), function(i) {
    it <- active$item[i]
    cn <- active$construct[i]
    loading_target <- NA_real_
    loading_off_max <- NA_real_
    off_fac <- NA_character_
    if (!is.null(P) && it %in% rownames(P)) {
      # The item's row of the loading matrix, with its factor names. A one
      # factor solution has a one column matrix; subsetting it without
      # drop = FALSE would return an unnamed number and lose the factor.
      li <- stats::setNames(as.numeric(P[it, , drop = FALSE]), colnames(P))
      tf <- if (is.na(factor_group[i])) NA_character_ else align[[factor_group[i]]]
      if (!is.na(tf) && tf %in% names(li)) {
        loading_target <- unname(li[tf])
        others <- li[setdiff(names(li), tf)]
      } else {
        others <- li
      }
      # With one factor there is no other factor to load on: the off target
      # loading does not exist (NA), which is not the same as an estimated
      # loading of zero, and no cross loading can be flagged.
      if (length(others)) {
        loading_off_max <- max(abs(others))
        off_fac <- names(others)[which.max(abs(others))]
      }
    }
    # Redundancy: largest absolute correlation with another active item in
    # the same construct (a near duplicate or local dependence signal).
    sib <- active$item[active$construct == cn & active$item != it]
    redundancy_max <- if (length(sib) && !is.null(R)) max(abs(R[it, sib])) else NA_real_

    th <- thresholds
    # Salience is judged by the size of the loading: a strongly reverse
    # keyed item (a large negative target loading) is not low loading. Its
    # sign is reported by flag_keying (sieve_item_keying()), below.
    flag_loading <- isTRUE(abs(loading_target) < th$load_low)
    flag_communality <- isTRUE(h2[it] < th$communality_low)
    flag_item_total <- isTRUE(itc[it] < th$item_total_low)
    flag_omega_drop <- isTRUE(omega_drop_delta[it] > 0)
    flag_redundancy <- isTRUE(redundancy_max > th$redundancy_high)
    # Cross loading: salient off target loading, within 0.10 of the target,
    # so the item looks complex and may belong elsewhere.
    # Against the size of the target loading: a negative target (an item
    # keyed against its factor) is compared by its magnitude, as the
    # off-target loading is.
    cross <- isTRUE(loading_off_max >= th$salient) &&
      isTRUE(loading_off_max >= (abs(loading_target) - 0.10))
    # The construct of the strongest off-target factor, returned regardless of
    # any threshold so the module can decide cross loading live.
    off_construct <- NA_character_
    if (!is.na(off_fac) && !is.null(align)) {
      hit <- names(align)[match(off_fac, align)]
      if (length(hit) && !is.na(hit)) off_construct <- hit
    }
    suggested <- if (cross) off_construct else NA_character_
    removal_signals <- sum(
      flag_loading, flag_communality, flag_item_total,
      flag_omega_drop, flag_redundancy
    )
    data.frame(
      item = it,
      construct = cn,
      factor_group = factor_group[i],
      loading_target = loading_target,
      loading_off_max = loading_off_max,
      off_construct = off_construct,
      communality = unname(h2[it]),
      complexity = unname(cmplx[it]),
      item_total = unname(itc[it]),
      item_total_n = unname(itc_n[it]),
      item_total_reason = unname(itc_reason[it]),
      omega_drop_delta = unname(omega_drop_delta[it]),
      alpha_drop_delta = unname(alpha_drop_delta[it]),
      redundancy_max = redundancy_max,
      removal_signals = removal_signals,
      cross_loading = cross,
      suggest_construct = suggested,
      flag_loading = flag_loading,
      flag_communality = flag_communality,
      flag_item_total = flag_item_total,
      flag_omega_drop = flag_omega_drop,
      flag_redundancy = flag_redundancy,
      stringsAsFactors = FALSE
    )
  })

  items_df <- do.call(rbind, rows)
  cons_df <- do.call(rbind, con_rows)
  # Keying: an item whose salient target loading is opposite in sign to the
  # other items of its factor. Its negative item-rest correlation and the
  # rise in omega without it follow from its sign, so they are not counted
  # as removal signals; the construct's note names it and the action.
  keyed <- sieve_item_keying(items_df, salient = thresholds$salient %||% 0.30, meta = meta)
  items_df$flag_keying <- keyed$flag
  # Every item-rest correlation and change if dropped of a construct that
  # holds such an item reads a total in which it subtracts, so those two
  # signals are withheld for all of the construct's items, not only for
  # the opposite one (sieve_item_live_flags()): otherwise they would point
  # at the correctly keyed items.
  items_df$keying_withheld <- items_df$construct %in% unique(items_df$construct[keyed$flag])
  items_df$removal_signals <- items_df$removal_signals -
    as.integer(items_df$keying_withheld & items_df$flag_omega_drop) -
    as.integer(items_df$keying_withheld & items_df$flag_item_total)
  cons_df$keying_note <- unname(keyed$notes[cons_df$construct])
  list(
    items = items_df,
    constructs = cons_df,
    n_obs = n_obs,
    k = k,
    factor_level = factor_level,
    factor_groups = factor_groups,
    basis_used = basis_used,
    basis_missing = setup$basis_missing,
    recomputed = computed$constructs,
    rng = rng,
    solution_reused = reused,
    efa_call = efa_call,
    efa_seconds = efa_seconds,
    status = efa_status,
    warnings = efa_warnings,
    reliability_missing = missing,
    reliability_aux = if (identical(missing, "fiml") && length(aux)) as.character(aux),
    reliability_only = reliability_only,
    notes = unique(c(notes, computed$worker_notes))
  )
}

#' The matrix and the factors of the Items tab's exploratory model
#'
#' What `sieve_item_diagnostics()` fits, resolved before any fit, so the
#' module can send the same DMAR call to a background R process
#' (`sieve_efa_point_call()` of `R`, `n_fit` and `k`): the included items
#' and their complete cases; the factors (one per construct, or, for a
#' one-construct study whose included items are split into two or more
#' facets, one per facet, the grouping Compare structures and the Bifactor tab use,
#' `sieve_structure_groups()`; an item of that construct without a facet
#' enters the model but has no target factor); and the correlation matrix,
#' the shared basis when it holds every included item and was computed on
#' the same complete cases (or is the saturated FIML matrix), the Pearson
#' matrix of the complete cases otherwise.
#'
#' @param data,meta,basis As for `sieve_item_diagnostics()`.
#' @return `NULL` with fewer than three included items; otherwise a list
#'   with `problem` (why the factor analysis cannot be fit, or `NULL`),
#'   `reliability_only` (`TRUE` when the problem is too few complete rows
#'   without a FIML basis, so each construct's reliability, which reads its
#'   own rows, can still be computed; `R` is then `NULL`), `active`, `items`,
#'   `X`, `n_obs`, `n_fit` (the `N` given to DMAR: the complete cases, a
#'   benchmark under the saturated FIML matrix, or DMAR's FIML N when fewer
#'   than three cases are complete; the point estimates do not depend on
#'   it), `notes`, `constructs`, `factor_level`, `factor_group`,
#'   `factor_groups`, `k`, `R`, `basis_used` and `basis_missing` (`"fiml"`
#'   for the saturated FIML matrix).
#' @author Ken Kelley
#' @noRd
sieve_item_efa_setup <- function(data, meta, basis = NULL) {
  active <- meta[meta$status == "active", , drop = FALSE]
  if (nrow(active) < 3L) {
    return(NULL)
  }
  items <- active$item
  # unclass() first: a column read from SPSS by haven carries a class whose
  # numeric conversion lives in haven, which need not be loaded.
  X <- sieve_diagnostics_frame(data, items)
  n_obs <- nrow(X)
  notes <- character(0)
  out <- list(problem = NULL, active = active, items = items, X = X, n_obs = n_obs)
  em_basis <- isTRUE(basis$ok) && identical(basis$missing, "fiml") &&
    is.matrix(basis$R) && all(items %in% rownames(basis$R))
  constructs <- unique(active$construct)
  grouping <- sieve_structure_groups(meta)
  factor_level <- if (identical(grouping$level, "facet")) "facet" else "construct"
  factor_group <- if (identical(factor_level, "facet")) {
    as.character(grouping$group)[match(items, grouping$items)]
  } else {
    as.character(active$construct)
  }
  factor_groups <- if (identical(factor_level, "facet")) grouping$groups else constructs
  if (n_obs <= ncol(X) && !em_basis) {
    # The factor analysis needs a correlation matrix of all included items:
    # of the complete rows, or the saturated FIML matrix of every row. Each
    # construct's reliability reads its own rows (FIML for continuous
    # items), so it is still given (`reliability_only`); a planned-missing
    # design, in which no row is complete, is the usual case.
    out$problem <- sprintf(
      paste0(
        "The exploratory factor analysis was not fit: %d row%s %s complete on the %d included items, too few for ",
        "a correlation matrix of the complete rows, and %s. It needs the saturated FIML correlation matrix of ",
        "every row as its basis (for continuous items, sieve_retention_cor(data, missing = \"fiml\"), the ",
        "matrix the app's FIML basis supplies), so the loading, h2, cross loading, complexity and max |r| ",
        "columns are empty. Each construct's reliability reads its own rows and is given."
      ),
      n_obs, if (n_obs == 1L) "" else "s", if (n_obs == 1L) "is" else "are", ncol(X),
      if (is.null(basis)) "no correlation matrix was supplied as the basis" else
        "the correlation matrix supplied as the basis is not the saturated FIML matrix of these items"
    )
    return(c(out, list(
      reliability_only = TRUE, n_fit = n_obs, notes = notes, constructs = constructs,
      factor_level = factor_level, factor_group = factor_group, factor_groups = factor_groups,
      k = length(factor_groups), R = NULL, basis_used = NA_character_, basis_missing = NULL
    )))
  }
  if (em_basis && n_obs <= ncol(X)) notes <- c(notes,
    "EM correlations use available continuous responses. Complete-case N is only a conservative reference; complete-case diagnostics may be undefined.")
  R <- NULL
  basis_used <- "pearson"
  basis_missing <- NULL
  if (!is.null(basis)) {
    fits <- isTRUE(basis$ok) && is.matrix(basis$R) && all(items %in% rownames(basis$R)) &&
      identical(as.integer(basis$n), as.integer(n_obs))
    if (fits) {
      R <- basis$R[items, items, drop = FALSE]
      basis_used <- basis$used
      basis_missing <- basis$missing
    } else {
      notes <- c(notes, paste(
        "The shared correlation matrix does not cover these items and cases,",
        "so Pearson correlations were computed here."
      ))
    }
  }
  if (is.null(R)) {
    R <- tryCatch(cor(X), error = function(e) NULL)
  }
  if (is.null(R)) {
    out$problem <- "Correlation matrix could not be computed."
    return(out)
  }
  c(out[c("problem", "active", "items", "X", "n_obs")], list(
    n_fit = if (n_obs >= 3L) n_obs else as.integer(basis$n_used %||% n_obs),
    notes = notes, constructs = constructs, factor_level = factor_level,
    factor_group = factor_group, factor_groups = factor_groups, k = length(factor_groups),
    R = R, basis_used = basis_used, basis_missing = basis_missing
  ))
}

#' One deterministic leave-one-item-out reliability refit
#'
#' On the rows the full coefficient read: under complete cases (ordered
#' items, or on request) `data` holds the rows complete on every item of
#' the construct, so the full and the reduced coefficients describe the
#' same respondents and dropping an item adds none; under FIML every row
#' with an observed item enters both fits, and a row observed only on the
#' dropped item contributes nothing to the reduced likelihood.
#'
#' @param item Item removed from the construct.
#' @param items Exact included construct membership.
#' @param data The construct's rows (`sieve_item_reliability_frame()`),
#'   with any auxiliary columns.
#' @param omega,alpha Full coefficients, whose unavailable targets stay unavailable.
#' @param ordered Whether the items are read as ordered.
#' @param missing `"listwise"` or `"fiml"`, as the full coefficient used.
#' @param aux Auxiliary columns of `data` for FIML.
#' @return The two reduced-item point coefficients.
#' @author Ken Kelley
#' @noRd
sieve_diagnostic_drop_point <- function(item, items, data, omega, alpha, ordered = FALSE,
                                        missing = "listwise", aux = NULL) {
  rest <- setdiff(items, item)
  X <- data[, c(rest, aux), drop = FALSE]
  c(
    omega = if (is.na(omega)) NA_real_ else as.numeric(sieve_item_omega_point(X, ordered = ordered, missing = missing, aux = aux)),
    alpha = if (is.na(alpha)) NA_real_ else as.numeric(sieve_alpha_point(X, missing = missing, aux = aux))
  )
}

#' The matrix of robust omega's denominator on the Items tab, in words
#'
#' Robust omega's denominator is the variance of the sum from the
#' unrestricted matrix of the construct's items: the unrestricted
#' polychoric correlations for ordered items; for continuous items fit by
#' full information maximum likelihood (some item value missing), the
#' saturated FIML covariance matrix of every row with an observed item (or
#' auxiliary); otherwise the covariances of the item scores on the rows
#' complete on the construct's items.
#'
#' @param ordered Whether the construct's items are read as ordered.
#' @param mixed Whether the construct mixes ordered and continuous items
#'   (read as continuous).
#' @param engine_missing The route the engine was called with
#'   (`sieve_item_reliability_frame()`): `"fiml"` or `"listwise"`.
#' @param aux The auxiliaries of the FIML fit, or `NULL`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_item_omega_matrix_text <- function(ordered, mixed = FALSE, engine_missing = "listwise", aux = NULL) {
  if (isTRUE(ordered)) return("unrestricted polychoric correlations")
  base <- if (identical(engine_missing, "fiml")) {
    paste0(
      "the saturated FIML covariance matrix of the items",
      if (length(aux)) sprintf(" (with %s as auxiliar%s)", sieve_and_text(aux), if (length(aux) == 1L) "y" else "ies") else ""
    )
  } else {
    "Pearson covariances of the item scores"
  }
  if (isTRUE(mixed)) paste0(base, " (the construct mixes ordered and continuous items, read as continuous)") else base
}

#' The matrices of robust omega's denominators on the Items tab, in words
#'
#' One matrix when every construct's denominator comes from the same kind
#' of matrix; otherwise each kind with the constructs it serves, so a
#' construct fit by FIML is not described by the covariances of the
#' complete rows.
#'
#' @param constructs The `constructs` table of `sieve_item_diagnostics()`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_item_omega_matrices_text <- function(constructs) {
  m <- as.character(constructs$omega_matrix)
  cn <- as.character(constructs$construct)
  keep <- !is.na(m)
  m <- m[keep]
  cn <- cn[keep]
  kinds <- unique(m)
  if (length(kinds) <= 1L) return(paste(kinds, collapse = ""))
  paste(vapply(kinds, function(k) sprintf("%s for %s", k, sieve_and_text(cn[m == k])), character(1)), collapse = "; ")
}

#' The rows and the treatment of one construct's reliability on the Items tab
#'
#' The Reliability tab's treatment, on the construct's own columns
#' (`sieve_reliability_spec()`): for continuous items under `missing =
#' "fiml"` (the default, DMAR's), every row with an observed item or
#' auxiliary, fit by full information maximum likelihood; when no item
#' value is missing, the complete-data route (the covariance matrix and N),
#' which gives the same estimate faster, auxiliaries or not (they inform
#' the item parameters only through rows with a missing item). For
#' ordered items (polychoric correlations need complete rows; FIML is not
#' defined for them) and under `missing = "listwise"`, the rows complete on
#' the construct's own items. The rows never depend on another construct's
#' items.
#'
#' @param data A data frame holding the item columns (and any auxiliary
#'   columns).
#' @param its The construct's included items.
#' @param ordered Whether the items are read as ordered.
#' @param missing `"fiml"` or `"listwise"`, the requested treatment.
#' @param aux Continuous auxiliary columns of `data` for FIML.
#' @return A list with `X` (the rows and columns to fit), `missing` (the
#'   treatment used: `"fiml"` or `"listwise"`), `engine_missing` (the route
#'   the engine is called with: `"listwise"` for complete data under FIML),
#'   `aux` (the auxiliaries used, or `NULL`), `n` (the rows used), and
#'   `n_complete` (the rows complete on the construct's items).
#' @author Ken Kelley
#' @noRd
sieve_item_reliability_frame <- function(data, its, ordered = FALSE, missing = "fiml", aux = NULL) {
  X <- data[, its, drop = FALSE]
  X[] <- lapply(X, sieve_as_number)
  seen <- rowSums(!is.na(as.matrix(X)))
  complete <- seen == length(its)
  fiml <- identical(missing, "fiml") && !isTRUE(ordered)
  aux <- if (fiml) intersect(setdiff(as.character(aux), its), names(data)) else character(0)
  if (!fiml) {
    return(list(
      X = X[complete, , drop = FALSE], missing = "listwise", engine_missing = "listwise", aux = NULL,
      n = sum(complete), n_complete = sum(complete)
    ))
  }
  if (length(aux)) {
    A <- data[, aux, drop = FALSE]
    A[] <- lapply(A, sieve_as_number)
    X <- cbind(X, A)
  }
  if (all(complete)) {
    # No item value is missing: the FIML estimates are the complete-data
    # ML estimates, auxiliaries or not (with every item observed the
    # likelihood of the items does not involve the saturated auxiliary
    # part), so the complete-data route (S and N) gives them faster.
    return(list(X = X[, its, drop = FALSE], missing = "fiml", engine_missing = "listwise", aux = NULL,
      n = nrow(X), n_complete = nrow(X)))
  }
  available <- rowSums(!is.na(as.matrix(X))) > 0L
  list(
    X = X[available, , drop = FALSE], missing = "fiml", engine_missing = "fiml",
    aux = if (length(aux)) aux, n = sum(available), n_complete = sum(complete)
  )
}

#' The items the Items diagnostics read, as numbers, on the complete cases
#'
#' @param data A data frame holding the item columns.
#' @param items The included items.
#' @return The complete-case numeric data frame.
#' @author Ken Kelley
#' @noRd
sieve_diagnostics_frame <- function(data, items) {
  X <- data[, items, drop = FALSE]
  X[] <- lapply(X, sieve_as_number)
  X[stats::complete.cases(X), , drop = FALSE]
}

#' What the Items diagnostics would compute, and the estimated wait
#'
#' The diagnostics are an exploratory factor analysis of the pool
#' (`DMAR::fa_exploratory()`, point estimates only, estimated by
#' `sieve_efa_point_wait()`: about 0.15 seconds for the nine bundled tests
#' at three factors, 0.3 for the 24 at five, 2.4 for 68 items at six) and,
#' for every construct whose block is not kept
#' (see `sieve_item_diagnostics()`), robust omega of its items, robust omega
#' without each item in turn (with four or more items) and one fit for the
#' residual correlations: `m + 2` one-factor fits for a construct of `m`
#' items. The factor analysis is fit again after every edit; the blocks are
#' kept. The wait is read from what is to be computed, so an edit that
#' touches one construct is estimated by the factor analysis and that
#' construct alone, and is what the run policy decides on
#' (`sieve_runs_by_itself(live = TRUE)`). See `sieve_diagnostics_wait()`
#' for the rates.
#'
#' A construct whose reliability is fit by FIML (continuous items with
#' missing values or auxiliaries; `sieve_item_reliability_frame()`) is
#' estimated at the FIML rate, which grows with its rows and item pairs.
#'
#' @param data,meta,ordered,missing,aux As for `sieve_item_diagnostics()`.
#' @param keep The environment the module keeps the blocks in, or `NULL`.
#' @param workers The socket workers the module may use.
#' @param efa `FALSE` when the factor analysis will not be fit (the EFA
#'   page's solution is read in its place).
#' @param R Optional: the correlation matrix the factor analysis will read
#'   (the shared basis when it is kept); the Pearson matrix of the complete
#'   cases otherwise. A bounded maximum likelihood screen of it
#'   (`sieve_efa_boundary_ks()`) says whether DMAR is expected to refuse the
#'   solution, which can take far longer than a proper fit.
#' @return A list with `seconds`, `seconds_one` (on one worker), `workers`
#'   (the workers the run will use), `blocks` (a data frame: `construct`,
#'   `items`, `ordered`, `fiml` (fit by FIML), `rows` (the rows its
#'   reliability reads), `kept`), `p`, `n`, `k` (the factors),
#'   `efa_seconds` (the factor analysis's part, 0 when it is not fit),
#'   `efa_refused` (the screen expects DMAR to refuse it) and
#'   `efa_background` (`TRUE` when that part alone is above two seconds,
#'   `sieve_auto_seconds()`, so it runs in a background R process); `NULL`
#'   with fewer than three included items.
#' @author Ken Kelley
#' @noRd
sieve_diagnostics_plan <- function(data, meta, keep = NULL, ordered = NULL, workers = 1L, efa = TRUE, R = NULL,
                                   missing = "fiml", aux = NULL) {
  active <- meta[meta$status == "active", , drop = FALSE]
  if (nrow(active) < 3L) {
    return(NULL)
  }
  items <- active$item
  if (is.null(ordered)) {
    ordered <- if ("type" %in% names(active)) active$item[active$type %in% c("ordered", "binary")] else character(0)
  }
  X <- sieve_diagnostics_frame(data, items)
  constructs <- unique(active$construct)
  grouping <- sieve_structure_groups(meta)
  k <- if (identical(grouping$level, "facet")) length(grouping$groups) else length(constructs)
  blocks <- do.call(rbind, lapply(constructs, function(cn) {
    its <- active$item[active$construct == cn]
    Xc_all <- data[, its, drop = FALSE]
    Xc_all[] <- lapply(Xc_all, sieve_as_number)
    treat <- length(its) > 0L && all(its %in% ordered)
    frame <- sieve_item_reliability_frame(data, its, ordered = treat, missing = missing, aux = aux)
    key <- sieve_diagnostics_block_key(its, frame$X, Xc_all, treat, frame$missing)
    held <- if (is.null(keep)) NULL else keep[[paste0("construct:", cn)]]
    data.frame(
      construct = cn, items = length(its), ordered = treat, fiml = identical(frame$engine_missing, "fiml"),
      rows = frame$n, kept = !is.null(held) && identical(held$key, key), stringsAsFactors = FALSE
    )
  }))
  todo <- blocks[!blocks$kept, , drop = FALSE]
  refused <- FALSE
  if (isTRUE(efa)) {
    if (is.null(R) || !all(items %in% rownames(R))) {
      R <- tryCatch(stats::cor(X), error = function(e) NULL)
    } else {
      R <- R[items, items, drop = FALSE]
    }
    refused <- !is.null(R) && nrow(X) > length(items) && isTRUE(sieve_efa_boundary_ks(R, k))
  }
  efa_seconds <- if (isTRUE(efa)) sieve_efa_point_wait(length(items), k, "matrix", refused = refused) else 0
  one <- sieve_diagnostics_wait(todo$items, todo$ordered, p = length(items), k = k, efa = efa, refused = refused,
    fiml = todo$fiml, n = todo$rows)
  # The module shares the per item fits over socket workers when they take
  # two seconds or more on one; the estimate follows the same rule.
  use <- if (is.finite(one - efa_seconds) && (one - efa_seconds) >= 2) max(1L, as.integer(workers)) else 1L
  list(
    seconds = sieve_diagnostics_wait(todo$items, todo$ordered, p = length(items), workers = use, k = k, efa = efa,
      refused = refused, fiml = todo$fiml, n = todo$rows),
    seconds_one = one, workers = use, blocks = blocks, p = length(items), n = nrow(X), k = k,
    efa_seconds = efa_seconds, efa_refused = refused,
    efa_background = isTRUE(efa) && is.finite(efa_seconds) && efa_seconds > sieve_auto_seconds()
  )
}

#' The identity of one construct's reliability block
#'
#' What the block reads: the construct's items, the rows its reliability
#' reads (its own rows, `sieve_item_reliability_frame()`, with any
#' auxiliary columns) and all cases (for the item-rest correlations),
#' whether they are read as ordered, and the missing data treatment.
#' Nothing of another construct enters it, so including or excluding an
#' item recomputes only the construct it belongs to.
#'
#' @param its The construct's included items.
#' @param Xc,Xc_all The reliability rows and the all-case item data.
#' @param ordered Whether the items are read as ordered.
#' @param missing The treatment used (`"fiml"` or `"listwise"`).
#' @return A list.
#' @author Ken Kelley
#' @noRd
sieve_diagnostics_block_key <- function(its, Xc, Xc_all, ordered, missing = "listwise") {
  list(
    items = its, reliability = sieve_data_key(Xc), all = sieve_data_key(Xc_all), ordered = isTRUE(ordered),
    missing = as.character(missing)
  )
}

#' The R that reproduces the Items tab's diagnostics
#'
#' Written to the Reproducible Code when the diagnostics are computed: the
#' shared correlation matrix of the included items, the exploratory factor
#' analysis by `DMAR::fa_exploratory()` with every argument written out
#' (maximum likelihood on the matrix, `S` with its `N`, direct oblimin from
#' the recorded seed, wrapped in [withr::with_seed()] under the generator the
#' app used, since DMAR seeds the generator in force), the loadings, h2 and
#' complexity read from DMAR's table, and each construct's robust omega
#' (observed denominator) with the coefficient if each item were dropped and
#' the largest single-factor residual correlation. When the loadings were read
#' from the EFA page's solution (the same estimates), its call is the one
#' written. The script's `active_items` and `meta` are the included item data
#' and the ledger.
#'
#' @param d The result of `sieve_item_diagnostics()`.
#' @param basis The shared basis it read (see `sieve_state_basis()`), or
#'   `NULL` for Pearson correlations computed here.
#' @param constructs,items The construct of each included item, and the
#'   items.
#' @param solution_call Optional: the DMAR call of the EFA page's solution
#'   that `d` read (`d$solution_reused`), written in place of this tab's.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_item_diagnostics_code <- function(d, basis, constructs, items, solution_call = NULL) {
  k <- d$k %||% length(unique(constructs))
  rng <- d$rng
  cor_code <- if (!is.null(basis) && isTRUE(basis$ok) && identical(basis$used, d$basis_used)) {
    sieve_retention_cor_code(basis)
  } else {
    c(
      "complete_rows <- stats::complete.cases(active_items)",
      "n_complete <- sum(complete_rows)",
      "R_items <- stats::cor(active_items[complete_rows, , drop = FALSE])"
    )
  }
  reused <- isTRUE(d$solution_reused) && !is.null(solution_call)
  call <- if (reused) {
    solution_call$rmsea <- NULL
    solution_call
  } else {
    d$efa_call %||% sieve_efa_point_call(k, R = NULL, n = d$n_obs, seed = rng$seed)
  }
  n_name <- if (identical(as.integer(call$args$N), as.integer(d$n_obs))) "n_complete" else as.character(call$args$N)
  # Too few complete rows and no FIML basis: the factor analysis was not
  # fit, and only the reliability is recorded.
  efa_lines <- if (isTRUE(d$reliability_only)) {
    c(
      sprintf("# Items tab: the exploratory factor analysis of the %d included items was not fit: too few", length(items)),
      "# rows are complete on every included item, and no saturated FIML correlation matrix was the basis",
      "# (for continuous items, sieve_retention_cor(active_items, missing = \"fiml\") would supply one)."
    )
  }
  fa_code <- if (is.null(efa_lines)) c(
    if (reused) {
      c(
        "# The loadings were read from the EFA page's solution, the same estimates (maximum",
        "# likelihood of the complete item responses, the same k and rotation); its call:"
      )
    },
    if (!is.null(call$args$seed)) {
      sprintf("# Rotation starts: seed = %d%s; DMAR sets it locally and restores the session's stream.",
        as.integer(call$args$seed),
        if (isTRUE(rng$supplied)) "" else " (SIEVER drew this one for the item pool so the run can be replayed)")
    },
    sieve_efa_dmar_code(call, result = "items_efa", data_name = "active_items", matrix_name = "R_items",
      n_name = n_name, rng_kind = rng$kind),
    sieve_efa_point_read_code("items_efa", "items")
  )
  fiml <- identical(d$reliability_missing %||% "fiml", "fiml")
  aux <- if (fiml) d$reliability_aux
  aux_lit <- if (length(aux)) sieve_r_literal(aux) else "NULL"
  aux_arg <- if (length(aux)) ", aux = items_aux" else ""
  cons <- d$constructs
  rel <- unlist(lapply(seq_len(nrow(cons)), function(i) {
    its <- items[constructs == cons$construct[i]]
    ordered <- identical(cons$treatment[i], "ordered")
    # As on the tab: no omega below three items (the one-factor model is
    # not identified with two).
    if (length(its) < 3L) {
      return(sprintf("# %s: %d included item%s, too few for omega (three or more are needed).",
        cons$construct[i], length(its), if (length(its) == 1L) "" else "s"))
    }
    c(
      sprintf("items_reliability[[%s]] <- items_construct(c(%s), ordered = %s)",
        encodeString(cons$construct[i], quote = "\""),
        paste(encodeString(its, quote = "\""), collapse = ", "), if (ordered) "TRUE" else "FALSE")
    )
  }), use.names = FALSE)
  c(
    if (!is.null(efa_lines)) efa_lines else c(
      sprintf("# Items tab: an exploratory factor analysis of the %d included items by DMAR::fa_exploratory()", length(items)),
      sprintf("# (maximum likelihood, %s, k = %d, one factor per %s) on the shared correlation",
        if (k > 1L) "direct oblimin rotation" else "no rotation", k,
        if (identical(d$factor_level, "facet")) "facet of the one construct" else "construct"),
      "# matrix, for the loadings, h2, complexity and cross loadings (point estimates; DMAR's intervals are not shown).",
      cor_code,
      fa_code,
      sprintf("items_loadings      # aligned to the %s by their largest mean absolute loading",
        if (identical(d$factor_level, "facet")) "facets" else "constructs"),
      "items_h2"
    ),
    if (fiml) {
      c(
        "# Robust omega (observed denominator) of each construct's items on that construct's own rows,",
        "# a point estimate: by full information maximum likelihood for continuous items (every row with",
        "# an observed item of the construct; the complete-data route, S and N, when none is missing),",
        "# on the rows complete on its own items for ordered items; the coefficient if each item were",
        "# dropped (four or more items), and the largest residual correlation of the one-factor model",
        "# (observed minus model implied correlations, from DMAR's fit)."
      )
    } else {
      c(
        "# Robust omega (observed denominator) of each construct's items on the rows complete on its",
        "# own items, a point estimate, the coefficient if each item were dropped (four or more items),",
        "# and the largest residual correlation of the one-factor model (observed minus model implied",
        "# correlations, from DMAR's fit)."
      )
    },
    if (length(aux)) sprintf("items_aux <- %s  # the declared auxiliaries, columns of the session data d", aux_lit),
    "robust_omega <- function(x, ordered, missing) {",
    "  r <- if (ordered) {",
    "    DMAR::reliability_omega_categorical(x, ci_method = \"none\")",
    "  } else if (missing == \"fiml\") {",
    sprintf("    DMAR::reliability_omega(data = x, missing = \"fiml\", ci_method = \"none\", denominator = \"observed\"%s)", aux_arg),
    "  } else {",
    "    DMAR::reliability_omega(S = stats::cov(x), N = nrow(x), ci_method = \"none\", denominator = \"observed\")",
    "  }",
    "  r$value[r$term == \"estimate\"]",
    "}",
    "items_construct <- function(its, ordered) {",
    "  x <- active_items[, its, drop = FALSE]",
    "  complete <- stats::complete.cases(x)",
    if (fiml) {
      c(
        "  # With every item observed the FIML estimates are the complete-data ones.",
        "  missing <- if (ordered || all(complete)) \"listwise\" else \"fiml\"",
        if (length(aux)) "  if (missing == \"fiml\") x <- cbind(x, d[, items_aux, drop = FALSE])",
        "  if (missing == \"listwise\") x <- x[complete, , drop = FALSE]"
      )
    } else {
      c("  missing <- \"listwise\"", "  x <- x[complete, , drop = FALSE]")
    },
    "  full <- robust_omega(x, ordered, missing)",
    "  dropped <- if (length(its) >= 4L) {",
    sprintf("    vapply(its, function(i) robust_omega(x[, setdiff(names(x), i), drop = FALSE], ordered, missing) - full, numeric(1))"),
    "  }",
    "  largest <- NA_real_  # three-item residual diagnostics are not interpreted",
    "  if (length(its) >= 4L) {",
    "    residual <- if (ordered) {",
    "      fit <- lavaan::cfa(paste(\"f =~\", paste(its, collapse = \" + \")), data = x, std.lv = TRUE,",
    "        estimator = \"WLSMV\", ordered = its, se = \"none\", test = \"standard\")",
    "      stats::cov2cor(lavaan::lavInspect(fit, \"sampstat\")$cov) -",
    "        stats::cov2cor(lavaan::lavInspect(fit, \"implied\")$cov)",
    "    } else {",
    "      attr(DMAR::reliability_omega(",
    "        data = x, missing = missing, ci_method = \"none\",",
    sprintf("        denominator = \"observed\"%s", if (length(aux)) ", aux = if (missing == \"fiml\") items_aux" else ""),
    "      ), \"residual_cor\")$matrix",
    "    }",
    "    residual[!upper.tri(residual)] <- NA",
    "    largest <- residual[which.max(abs(residual))]",
    "  }",
    "  list(robust_omega = full, change_if_dropped = dropped, largest_residual = largest)",
    "}",
    "items_reliability <- list()",
    rel,
    "items_reliability"
  )
}

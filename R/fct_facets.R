# Shared construct and facet membership. A facet belongs to its construct;
# its display label alone is never an analysis or cache identity.

#' Identify a facet within its parent construct
#' @param construct,facet Character vectors of parent and facet labels.
#' @return Collision-free length-prefixed keys, or NA for unassigned pairs.
#' @author Ken Kelley
#' @noRd
sieve_facet_id <- function(construct, facet) {
  n <- max(length(construct), length(facet))
  if (!length(construct) || !length(facet)) return(character(0))
  construct <- enc2utf8(rep_len(as.character(construct), n))
  facet <- enc2utf8(rep_len(as.character(facet), n))
  valid <- !is.na(construct) & nzchar(construct) & !is.na(facet) & nzchar(facet)
  out <- rep(NA_character_, length(construct))
  out[valid] <- paste0(
    nchar(construct[valid], type = "bytes"), ":", construct[valid],
    nchar(facet[valid], type = "bytes"), ":", facet[valid]
  )
  out
}

#' The included items of each nested facet
#' @param meta The item ledger.
#' @param columns Optional names of available item columns.
#' @return A named list with id, construct, facet, label, items, types, and k.
#' @author Ken Kelley
#' @noRd
sieve_facet_groups <- function(meta, columns = NULL) {
  if (!"facet" %in% names(meta)) return(list())
  ids <- sieve_facet_id(meta$construct, meta$facet)
  keep <- meta$status == "active" & !is.na(ids)
  if (!is.null(columns)) keep <- keep & meta$item %in% columns
  active <- meta[keep, , drop = FALSE]
  ids <- ids[keep]
  out <- lapply(unique(ids), function(id) {
    rows <- active[ids == id, , drop = FALSE]
    list(
      id = id, construct = as.character(rows$construct[1]),
      facet = as.character(rows$facet[1]),
      label = sprintf("%s: %s", rows$construct[1], rows$facet[1]),
      items = as.character(rows$item), types = as.character(rows$type), k = nrow(rows)
    )
  })
  names(out) <- unique(ids)
  out
}

#' Corrected Item-Rest Correlations on Available Item Means
#'
#' Each scored item is correlated with the mean of the other included
#' items. A rest mean needs at least one observed other item; Pearson r
#' uses rows with both that mean and the focal item observed. This is the
#' established construct item-rest convention, also used for facets.
#' @details Results are descriptive point estimates, without rounding or
#'   a design-adjusted interval. Reversals must already be applied. Undefined
#'   correlations retain their analytic pair count and reason.
#' @param data Scored item responses, with reversals already applied.
#' @param items The included items in one construct or facet.
#' @return A data frame with item, r, n (usable pairs), and reason.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' sieve_item_rest(items, c("x1", "x2", "x3"))
#' @export
sieve_item_rest <- function(data, items) {
  X <- sieve_reliability_data(data, items)
  rows <- lapply(items, function(item) {
    others <- setdiff(items, item)
    r <- NA_real_
    n <- 0L
    reason <- if (!length(others)) "Only one included item; no other item forms a rest score." else ""
    if (length(others)) {
      rest <- rowMeans(X[, others, drop = FALSE], na.rm = TRUE)
      pairs <- is.finite(X[[item]]) & is.finite(rest)
      n <- sum(pairs)
      if (n < 2L) {
        reason <- "Fewer than two observed item-rest pairs."
      } else if (stats::sd(X[[item]][pairs]) == 0 || stats::sd(rest[pairs]) == 0) {
        reason <- "The item or its rest score has zero variance among the usable pairs."
      } else {
        r <- stats::cor(X[[item]][pairs], rest[pairs])
      }
    }
    data.frame(
      item = item, r = r, n = as.integer(n), reason = reason,
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) return(data.frame(item = character(), r = numeric(), n = integer(), reason = character()))
  do.call(rbind, rows)
}

#' Cheap Facet Diagnostics, Separate From Construct Model Fits
#'
#' @description
#' Correlates each included item with the rest of its facet (the facet's
#' other included items), a quick check of how an item sits within its facet
#' that fits no model and so does not wait for a construct's model. An
#' excluded item, an item without a facet, and an item whose facet is too
#' small are listed with the reason instead of a value.
#'
#' @details Results are descriptive point estimates, without rounding or
#'   a design-adjusted interval. Reversals must already be applied. Undefined
#'   correlations retain their analytic pair count and reason.
#' @param data Scored item responses.
#' @param meta Ledger with `item`, `construct`, `facet`, and `status`. A facet
#'   is identified within its parent construct, not by its label alone.
#' @param keep Optional cache environment shared across calls.
#' @return A data frame with item, facet_id, facet_item_total,
#'   facet_item_total_n, and facet_item_total_reason, including excluded items.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' # One construct, Ability, with three facets of three tests each.
#' meta <- data.frame(item = names(items), label = names(items), construct = "Ability",
#'   facet = rep(c("Visual", "Textual", "Speed"), each = 3L), type = "continuous",
#'   status = "active", reverse = FALSE, stringsAsFactors = FALSE)
#' sieve_facet_item_rest(items, meta)
#' @export
sieve_facet_item_rest <- function(data, meta, keep = NULL) {
  ids <- if ("facet" %in% names(meta)) sieve_facet_id(meta$construct, meta$facet) else rep(NA_character_, nrow(meta))
  out <- data.frame(
    item = meta$item, facet_id = ids, facet_item_total = NA_real_,
    facet_item_total_n = 0L,
    facet_item_total_reason = ifelse(meta$status != "active", "Item is excluded.",
      ifelse(is.na(ids), "No facet is assigned within a construct.", "Item column is unavailable.")
    ),
    stringsAsFactors = FALSE
  )
  for (group in sieve_facet_groups(meta, names(data))) {
    compute <- function() sieve_item_rest(data, group$items)
    block <- if (is.null(keep)) compute() else sieve_keep(
      keep, paste0("facet:", group$id),
      list(items = group$items, data = sieve_data_key(data[, group$items, drop = FALSE])), compute
    )
    at <- match(block$item, out$item)
    out$facet_item_total[at] <- block$r
    out$facet_item_total_n[at] <- block$n
    out$facet_item_total_reason[at] <- block$reason
  }
  out
}

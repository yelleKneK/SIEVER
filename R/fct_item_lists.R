# Selecting many items at once on the Ingest tab: a pasted list of items to
# include or exclude, and the originals of reverse-coded copies saved in the
# same file. Both are read here, without Shiny, so the grid's buttons and the
# tests use the same rules; the change itself is made by
# sieve_set_item_status(), which records it in the reproducible code.

#' Quotation marks around the entries of a pasted list, dropped
#'
#' @param x A character vector.
#' @return `x` trimmed, without the straight or curly quotation marks or
#'   backticks at either end.
#' @author Ken Kelley
#' @noRd
sieve_strip_quotes <- function(x) {
  quotes <- "[\"'\u201c\u201d\u2018\u2019`]"
  trimws(gsub(sprintf("^%s+|%s+$", quotes, quotes), "", trimws(x)))
}

#' The entries of a pasted list of items
#'
#' A list copied from a spreadsheet column, a text file or a sentence: the
#' entries are separated by line breaks or tabs, and the quotation marks
#' around an entry are dropped. Commas, semicolons and spaces inside an entry
#' are kept here, since an Item Name or a wording can hold them;
#' `sieve_match_item_list()` splits an entry at its commas or semicolons, and
#' then at its spaces, only when the whole entry names no item.
#'
#' @param text The pasted text (a character vector is read as lines).
#' @return A character vector of the non-empty entries, in order.
#' @author Ken Kelley
#' @noRd
sieve_item_list_entries <- function(text) {
  text <- paste(as.character(text %||% character(0)), collapse = "\n")
  if (!nzchar(trimws(text))) {
    return(character(0))
  }
  parts <- sieve_strip_quotes(unlist(strsplit(text, "[\r\n\t]+"), use.names = FALSE))
  parts[nzchar(parts)]
}

#' Match a pasted list of items to the item ledger
#'
#' Each entry is matched to one item, trying in turn its Variable Name as
#' written, its Item Name as written, its Variable Name ignoring case, its
#' Item Name ignoring case and runs of spaces, and its wording ignoring case
#' and runs of spaces. The first of these that names exactly one item
#' decides; an entry that names two or more items at that step is reported
#' as ambiguous and matched to none. An entry that names no item as a whole
#' and holds commas or semicolons is read as a list of entries (as
#' `q1, q2; q3`), each matched in the same way, so a wording that holds a
#' comma still matches as a whole first. An entry that names no item and
#' holds spaces is read as several names (as `q1 q2 q3`, pasted from a
#' sentence) when every one of them names exactly one item.
#'
#' @param entries The entries (`sieve_item_list_entries()`).
#' @param meta The item ledger (`item`, and `label` and `wording` when
#'   present).
#' @return A list with `items` (the matched items, once each, in ledger
#'   order), `matches` (a data frame of `entry` and `item`, one row per
#'   matched name), `unmatched` and `ambiguous` (the entries matched to no
#'   item, each once).
#' @author Ken Kelley
#' @noRd
sieve_match_item_list <- function(entries, meta) {
  entries <- as.character(entries)
  none <- list(
    items = character(0),
    matches = data.frame(entry = character(0), item = character(0), stringsAsFactors = FALSE),
    unmatched = unique(entries), ambiguous = character(0)
  )
  if (is.null(meta) || !nrow(meta) || !length(entries)) {
    return(none)
  }
  items <- as.character(meta$item)
  labels <- if ("label" %in% names(meta)) as.character(meta$label) else rep(NA_character_, length(items))
  wording <- if ("wording" %in% names(meta)) as.character(meta$wording) else rep(NA_character_, length(items))
  squash <- function(x) tolower(gsub("[[:space:]]+", " ", trimws(x)))
  labels_sq <- squash(labels)
  wording_sq <- squash(wording)
  steps <- list(
    function(e) which(items == e),
    function(e) which(!is.na(labels) & labels == e),
    function(e) which(tolower(items) == tolower(e)),
    function(e) which(!is.na(labels) & labels_sq == squash(e)),
    function(e) which(!is.na(wording) & nzchar(wording_sq) & wording_sq == squash(e))
  )
  # The rows the first deciding step names: none, one, or several.
  lookup <- function(e) {
    for (step in steps) {
      hit <- step(e)
      if (length(hit)) {
        return(hit)
      }
    }
    integer(0)
  }
  matched_entry <- character(0)
  matched_row <- integer(0)
  unmatched <- character(0)
  ambiguous <- character(0)
  for (e in entries) {
    hit <- lookup(e)
    if (length(hit) == 1L) {
      matched_entry <- c(matched_entry, e)
      matched_row <- c(matched_row, hit)
      next
    }
    if (length(hit) > 1L) {
      ambiguous <- c(ambiguous, e)
      next
    }
    if (grepl("[,;]", e)) {
      parts <- sieve_strip_quotes(strsplit(e, "[,;]+")[[1L]])
      inner <- sieve_match_item_list(parts[nzchar(parts)], meta)
      matched_entry <- c(matched_entry, inner$matches$entry)
      matched_row <- c(matched_row, match(inner$matches$item, items))
      unmatched <- c(unmatched, inner$unmatched)
      ambiguous <- c(ambiguous, inner$ambiguous)
      next
    }
    words <- strsplit(e, "[[:space:]]+")[[1L]]
    words <- words[nzchar(words)]
    if (length(words) > 1L) {
      hits <- lapply(words, lookup)
      if (all(lengths(hits) == 1L)) {
        matched_entry <- c(matched_entry, words)
        matched_row <- c(matched_row, unlist(hits, use.names = FALSE))
        next
      }
    }
    unmatched <- c(unmatched, e)
  }
  rows <- sort(unique(matched_row))
  list(
    items = items[rows],
    matches = data.frame(entry = matched_entry, item = items[matched_row], stringsAsFactors = FALSE),
    unmatched = unique(unmatched),
    ambiguous = unique(ambiguous)
  )
}

#' The markers that name a reverse-coded copy of an item
#'
#' The words a data file adds to an item's name for its reverse-coded copy,
#' as a suffix (`q5_r`, `q5r`, `q5.rev`, `q5_reversed`, `q5_rc`) or a prefix
#' (`r_q5`, `rev_q5`, `rq5`), compared ignoring case.
#'
#' @return A list with `markers` and `separators`.
#' @author Ken Kelley
#' @noRd
sieve_reverse_name_markers <- function() {
  list(
    markers = c("r", "rev", "rc", "rec", "recode", "recoded", "reverse", "reversed"),
    separators = c("_", ".", "-", " ")
  )
}

#' Originals of reverse-coded copies saved in the same file
#'
#' A file often holds an item twice: as administered (`q5`) and reverse
#' coded beside it (`q5_r`). The two carry one response, so only one belongs
#' in the analysis: the one keyed with its construct. A pair is found from
#' the names: the copy's name is the original's with a reverse marker added
#' (`sieve_reverse_name_markers()`; with no separator only for a suffix, or
#' for the one-letter prefix `r`). The values then say whether the copy runs
#' opposite to the original: their correlation, on the cases that answered
#' both, is negative, or positive when exactly one of the two is reverse
#' scored in this session (the ledger's `reverse`).
#'
#' Which of the two is keyed with the construct is read, as in
#' `sieve_item_copies()`, from each one's correlation with the mean of its
#' construct's other included items (the pair left out), the convention of
#' a corrected item-total correlation: the one whose correlation is positive
#' is keyed with it (every item paired by name with either member is left
#' out of the mean). That needs an assigned construct with at least two
#' other included items for at least one member; when both members' are
#' known and of the same sign, neither is the other reversed against its
#' construct, and the pair is not proposed. Without either, the name decides: a file reverse codes
#' the items worded against their construct, so the copy is taken to be the
#' keyed one, and the note says the name decided. A pair is proposed (the
#' original excluded, the copy kept) when both are included, their values run
#' opposite, and the copy is the keyed one or the construct cannot tell.
#' Every other pair is returned with the reason it is not proposed. Nothing
#' is read from the size of a correlation; they are returned so that the
#' researcher sees them (an exact reflection gives -1).
#'
#' @param items The item data (one column per item).
#' @param meta The item ledger (`item`, `status`, and `construct` and
#'   `reverse` when present).
#' @return A data frame with one row per pair: `original`, `copy`, `r` (the
#'   correlation of the values now held, full precision), `n` (the cases
#'   that answered both), `flipped` (exactly one of the two is reverse
#'   scored in this session), `construct`, `r_original` and `r_copy` (each
#'   one's correlation with the mean of the construct's other included
#'   items, or `NA`), `keyed` (`"copy"`, `"original"`, or `NA` when the
#'   construct cannot tell), `proposed` (logical) and `reason` (why a pair
#'   is not proposed, or `NA`). No rows when no name pairs.
#' @author Ken Kelley
#' @noRd
sieve_reversed_originals <- function(items, meta) {
  empty <- data.frame(
    original = character(0), copy = character(0), r = numeric(0), n = integer(0),
    flipped = logical(0), construct = character(0), r_original = numeric(0), r_copy = numeric(0),
    keyed = character(0), proposed = logical(0), reason = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(meta) || !nrow(meta)) {
    return(empty)
  }
  names_all <- as.character(meta$item)
  lower <- tolower(names_all)
  spec <- sieve_reverse_name_markers()
  pairs <- list()
  for (i in seq_along(names_all)) {
    base <- lower[i]
    suffixed <- c(
      paste0(base, spec$markers),
      as.vector(outer(spec$separators, spec$markers, function(s, m) paste0(base, s, m)))
    )
    prefixed <- c(
      paste0("r", base),
      as.vector(outer(spec$markers, spec$separators, function(m, s) paste0(m, s, base)))
    )
    hit <- setdiff(which(lower %in% c(suffixed, prefixed)), i)
    for (j in hit) pairs[[length(pairs) + 1L]] <- c(names_all[i], names_all[j])
  }
  if (!length(pairs)) {
    return(empty)
  }
  status <- stats::setNames(as.character(meta$status), names_all)
  reversed <- if ("reverse" %in% names(meta)) {
    stats::setNames(meta$reverse %in% TRUE, names_all)
  } else {
    stats::setNames(rep(FALSE, length(names_all)), names_all)
  }
  construct_of <- stats::setNames(
    if ("construct" %in% names(meta)) as.character(meta$construct) else rep(NA_character_, length(names_all)),
    names_all
  )
  numeric_item <- function(v) !is.null(items) && v %in% names(items) && is.numeric(items[[v]])
  active <- names_all[status == "active"]
  # Every item paired by name with a member of `pair`, and with those, so
  # that a second copy of one original (q5_r and rev_q5) never enters the
  # construct mean the first is read against.
  linked <- function(pair) {
    g <- pair
    repeat {
      grow <- unique(c(g, unlist(Filter(function(q) any(q %in% g), pairs), use.names = FALSE)))
      if (length(grow) == length(g)) {
        return(g)
      }
      g <- grow
    }
  }
  # An item's correlation with the mean of its construct's other included
  # items, every item linked to the pair by name left out.
  with_rest <- function(it, pair) {
    cn <- construct_of[[it]]
    if (!numeric_item(it) || !sieve_construct_assigned(cn)) {
      return(NA_real_)
    }
    rest <- setdiff(active[construct_of[active] %in% cn], linked(pair))
    rest <- rest[vapply(rest, numeric_item, logical(1))]
    if (length(rest) < 2L) {
      return(NA_real_)
    }
    m <- rowMeans(as.matrix(as.data.frame(lapply(items[rest], as.numeric))), na.rm = TRUE)
    m[!is.finite(m)] <- NA_real_
    suppressWarnings(stats::cor(as.numeric(items[[it]]), m, use = "pairwise.complete.obs"))
  }
  rows <- lapply(pairs, function(p) {
    a <- p[1L]
    b <- p[2L]
    r <- NA_real_
    n <- 0L
    if (numeric_item(a) && numeric_item(b)) {
      both <- !is.na(items[[a]]) & !is.na(items[[b]])
      n <- sum(both)
      if (n >= 2L) {
        r <- suppressWarnings(stats::cor(as.numeric(items[[a]][both]), as.numeric(items[[b]][both])))
      }
    }
    flipped <- xor(reversed[[a]], reversed[[b]])
    opposite <- is.finite(r) && (if (flipped) r > 0 else r < 0)
    r_a <- with_rest(a, p)
    r_b <- with_rest(b, p)
    # The member keyed with its construct: the one whose correlation with
    # the construct's other items is positive. With one member's correlation
    # known, it decides; with both known and of the same sign, the two do not
    # run opposite with respect to the construct, and neither is keyed.
    keyed <- if (is.finite(r_a) && is.finite(r_b)) {
      if (sign(r_a) != sign(r_b)) (if (r_b > 0) "copy" else "original") else NA_character_
    } else if (is.finite(r_b)) {
      if (r_b > 0) "copy" else "original"
    } else if (is.finite(r_a)) {
      if (r_a > 0) "original" else "copy"
    } else {
      NA_character_
    }
    from <- if (is.finite(r_b)) b else a
    known <- is.finite(c(r_a, r_b))
    r_shown <- paste(sprintf("%s, %.2f", c(a, b)[known], c(r_a, r_b)[known]), collapse = "; ")
    reason <- if (!identical(status[[a]], "active")) {
      sprintf("%s is already excluded", a)
    } else if (!identical(status[[b]], "active")) {
      sprintf("the copy %s is excluded", b)
    } else if (!is.finite(r)) {
      "their values could not be compared"
    } else if (!opposite) {
      sprintf("the values of %s do not run opposite to those of %s", b, a)
    } else if (identical(keyed, "original")) {
      sprintf(
        "%s, the original, is the one keyed with %s (correlation with the mean of its other included items: %s), so the copy is the one to exclude",
        a, construct_of[[from]], r_shown
      )
    } else if (is.na(keyed) && all(known)) {
      sprintf(
        "%s and %s both correlate %s with the mean of their construct's other included items (%s), so neither runs against its construct as the other's reversal would",
        a, b, if (r_a > 0) "positively" else "negatively", r_shown
      )
    } else {
      NA_character_
    }
    data.frame(
      original = a, copy = b, r = r, n = as.integer(n), flipped = flipped,
      construct = if (is.na(keyed)) NA_character_ else construct_of[[from]],
      r_original = r_a, r_copy = r_b, keyed = keyed,
      proposed = is.na(reason), reason = reason, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  # A chain of names (q5, q5_r, q5_r_r): the copy of a proposed pair is not
  # also the original of another, whose exclusion would leave the copy of a
  # copy, the original's values again.
  go <- out$proposed
  chained <- go & out$original %in% out$copy[go]
  if (any(chained)) {
    first <- out$original[go][match(out$original[chained], out$copy[go])]
    out$reason[chained] <- sprintf("%s is itself the reverse-coded copy of %s", out$original[chained], first)
    out$proposed[chained] <- FALSE
  }
  out[order(!out$proposed, out$original, out$copy), , drop = FALSE]
}

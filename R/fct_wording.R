# fct_wording.R
# Deterministic, fully local item-wording quality checks, following the
# item-writing guidelines summarized by Bandalos (2018, ch. 5): keep items
# short and readable, ask one thing at a time, avoid negations (especially
# double negatives), avoid absolute and vague quantifiers, and balance
# reverse-keyed items deliberately. Everything here is plain string work: no
# model, no network, no data leaving the machine. The flags are candidates
# for revision, not verdicts; wording problems are judged by reading the
# item, and these checks only say where to look first.

# Marker vocabularies, lowercase. Word-boundary matching is applied when the
# markers are used, so "or" does not fire inside "correct".
.sieve_negations <- c(
  "not", "no", "never", "none", "nothing", "nobody", "neither", "nor",
  "cannot", "can't", "don't", "doesn't", "didn't", "won't", "wouldn't",
  "shouldn't", "couldn't", "isn't", "aren't", "wasn't", "weren't", "without"
)
.sieve_absolutes <- c(
  "always", "never", "all", "none", "every", "everyone", "everybody",
  "nothing", "nobody", "only", "completely", "totally", "absolutely"
)
.sieve_vague <- c(
  "often", "sometimes", "occasionally", "rarely", "seldom", "frequently",
  "generally", "usually", "somewhat", "fairly", "several", "many", "few"
)

#' Count syllables in one word by vowel-group heuristic
#'
#' Counts maximal vowel groups (a, e, i, o, u, y), subtracting a trailing
#' silent e. A heuristic, not a dictionary: adequate for a reading-grade
#' estimate that is itself only a screening aid.
#'
#' @param w A single lowercase word (letters only).
#' @return An integer syllable count, at least 1.
#' @author Ken Kelley
#' @noRd
sieve_syllables <- function(w) {
  if (!nzchar(w)) return(0L)
  groups <- gregexpr("[aeiouy]+", w)[[1]]
  n <- if (identical(as.integer(groups[1]), -1L)) 0L else length(groups)
  # A trailing silent e ("scale", "operate") closes no syllable of its own,
  # unless it is the only vowel ("the", "be").
  if (n > 1L && grepl("[^aeiouy]e$", w)) n <- n - 1L
  max(n, 1L)
}

#' Tokenize a wording into lowercase word tokens
#' @param text The item wording.
#' @return A character vector of lowercase tokens (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_tokens <- function(text) {
  x <- enc2utf8(tolower(text))
  # Typographic apostrophes (right and left single quotation marks, the
  # modifier letter apostrophe, the prime), as pasted from a word processor,
  # are the apostrophe of a contraction: "don\u2019t" is "don't".
  x <- gsub("[\u2018\u2019\u02bc\u2032]", "'", x, perl = TRUE)
  # Letters of any script stay inside a word ("d\u00e9tendu" is one word),
  # so a word is a run of Unicode letters and apostrophes.
  x <- gsub("[^\\p{L}' ]", " ", x, perl = TRUE)
  tok <- strsplit(trimws(gsub(" +", " ", x)), " ", fixed = TRUE)[[1]]
  tok <- tok[nzchar(tok)]
  # "can not" is the negation "cannot" written as two words: one token, so
  # both spellings are found, reported, and counted the same way.
  at <- which(tok[-length(tok)] == "can" & tok[-1L] == "not")
  if (length(at)) {
    tok[at] <- "cannot"
    tok <- tok[-(at + 1L)]
  }
  tok
}

#' Deterministic Wording-Quality Checks for the Active Items
#'
#' Applies the Bandalos (2018) item-writing guidelines that can be computed
#' from the text alone. For each active item with a non-empty wording:
#' word count; an approximate Flesch-Kincaid grade level (0.39 words per
#' sentence + 11.8 syllables per word - 15.59, with syllables from a
#' vowel-group heuristic); double-barreled markers (a conjunction joining
#' two predicates, or "and/or"); negations, with double negatives flagged
#' separately; absolute quantifiers; and vague frequency quantifiers.
#' Returns one row per item plus a `flags` count so the worst items sort
#' first. All checks are heuristics on the text; none reads the data.
#'
#' @details These English-language rules are deterministic prompts for human
#'   review. They do not establish reading comprehension, content coverage, or
#'   empirical item functioning; no model or external provider is invoked.
#' @param meta The item ledger (needs `item`, `label`, `construct`,
#'   `status`, `wording`, `reverse`).
#' @return A data frame with one row per active worded item: `item`,
#'   `label`, `construct`, `words`, `grade`, `double_barreled`, `negation`,
#'   `double_negative`, `absolute`, `vague`, `flags`, and the marker words
#'   found (`found`, a comma-separated string). Items without wording are
#'   omitted; if none carry wording the frame has zero rows.
#' @author Ken Kelley
#' @examples
#' meta <- data.frame(item = paste0("q", 1:3), label = paste0("Q", 1:3), construct = "Sociability",
#'   status = "active", reverse = c(FALSE, TRUE, FALSE), stringsAsFactors = FALSE)
#' meta$wording <- c(
#'   "I enjoy meeting new people.",
#'   "I do not like not being invited to parties.",
#'   "I always talk to strangers and often start conversations."
#' )
#' sieve_wording_checks(meta)[, c(
#'   "item", "words", "negation", "double_negative", "absolute", "vague", "flags", "found"
#' )]
#' @export
sieve_wording_checks <- function(meta) {
  active <- meta[meta$status == "active", , drop = FALSE]
  has_w <- !is.na(active$wording) & nzchar(trimws(active$wording))
  active <- active[has_w, , drop = FALSE]
  empty <- data.frame(
    item = character(0), label = character(0), construct = character(0),
    words = integer(0), grade = numeric(0),
    double_barreled = logical(0), negation = logical(0),
    double_negative = logical(0), absolute = logical(0), vague = logical(0),
    flags = integer(0), found = character(0), stringsAsFactors = FALSE
  )
  if (!nrow(active)) return(empty)

  rows <- lapply(seq_len(nrow(active)), function(i) {
    text <- active$wording[i]
    tok <- sieve_tokens(text)
    n_words <- length(tok)
    n_sent <- max(1L, lengths(regmatches(text, gregexpr("[.!?]+", text))))
    n_syll <- sum(vapply(gsub("'", "", tok), sieve_syllables, integer(1)))
    grade <- if (n_words > 0) {
      0.39 * (n_words / n_sent) + 11.8 * (n_syll / n_words) - 15.59
    } else NA_real_

    neg_hits <- intersect(tok, .sieve_negations)
    abs_hits <- intersect(tok, .sieve_absolutes)
    vag_hits <- intersect(tok, .sieve_vague)
    n_neg <- sum(tok %in% .sieve_negations)
    # Double-barreled: "and/or" anywhere, or a coordinating conjunction with
    # enough words on each side to be joining two predicates rather than a
    # fixed phrase ("ups and downs" trips it too; the reader decides).
    conj_pos <- which(tok %in% c("and", "or"))
    barreled <- grepl("and/or", tolower(text), fixed = TRUE) ||
      any(conj_pos >= 3L & conj_pos <= n_words - 2L)

    hits <- c(neg_hits, abs_hits, vag_hits)
    data.frame(
      item = active$item[i],
      label = active$label[i],
      construct = active$construct[i],
      words = n_words,
      grade = grade,
      double_barreled = barreled,
      negation = n_neg >= 1L,
      double_negative = n_neg >= 2L,
      absolute = length(abs_hits) > 0L,
      vague = length(vag_hits) > 0L,
      # The reading-grade flag needs a real sentence to be meaningful; the
      # Flesch-Kincaid formula is unstable on fragments, so it only counts
      # for wordings of eight words or more.
      flags = as.integer(barreled) + as.integer(n_neg >= 1L) +
        as.integer(n_neg >= 2L) + as.integer(length(abs_hits) > 0L) +
        as.integer(length(vag_hits) > 0L) +
        as.integer(n_words > 20L) +
        as.integer(n_words >= 8L && is.finite(grade) && grade > 12),
      found = paste(unique(hits), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

#' Reverse-Keyed Balance per Construct
#'
#' Summarizes, per construct, how many active items are reverse keyed.
#' Bandalos (2018) and the method-effects literature recommend a deliberate
#' choice: either no reverse-keyed items (avoiding the method factor they
#' induce) or enough of them to detect acquiescent responding, never a
#' single stray one.
#'
#' @details This describes keying flags only. It does not transform responses
#'   or diagnose acquiescence or method effects.
#' @param meta Ledger with `construct`, `status`, and logical `reverse`.
#' @return A data frame with `construct`, `k` (active items), `reversed`,
#'   and `note`. Active items without a construct (a construct cell that is
#'   missing, blank, or `Unassigned`) are counted together in one row,
#'   `"Unassigned"`, the label [sieve_item_coverage()] uses.
#' @author Ken Kelley
#' @examples
#' meta <- data.frame(item = paste0("q", 1:6), construct = rep(c("Sociability", "Calm"), each = 3L),
#'   status = "active", reverse = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE),
#'   stringsAsFactors = FALSE)
#' sieve_reverse_balance(meta)
#' @export
sieve_reverse_balance <- function(meta) {
  active <- meta[meta$status %in% "active", , drop = FALSE]
  # An item without a construct is counted in its own group; comparing a
  # missing construct with == would make every count missing.
  construct <- as.character(active$construct)
  construct[!sieve_construct_assigned(construct)] <- "Unassigned"
  cons <- unique(construct)
  rows <- lapply(cons, function(cn) {
    sel <- construct %in% cn
    k <- sum(sel)
    rev_n <- sum(active$reverse[sel] %in% c(TRUE, "TRUE", "yes"))
    note <- if (rev_n == 0L) {
      "none reversed (acquiescence undetectable)"
    } else if (rev_n == 1L) {
      "a single reversed item (prone to a stray method artifact)"
    } else {
      "reversed items present"
    }
    data.frame(
      construct = cn, k = k, reversed = rev_n, note = note,
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      construct = character(0), k = integer(0), reversed = integer(0),
      note = character(0), stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

#' Detect the measurement level of an item
#'
#' Classifies a single variable as binary, ordered, or continuous from its
#' distinct non missing values. The result is a proposal the user confirms or
#' overrides; it is never applied silently. The rule: two distinct values is
#' binary; an integer valued variable with three or four distinct values (at
#' most `max_ordered`) is ordered; five or more distinct values, or any non
#' integer value, is continuous. Rhemtulla, Brosseau-Liard and Savalei (2012)
#' found that with five or more categories estimation that treats the items as
#' continuous (Pearson covariances, ML or MLR with full information maximum
#' likelihood) performs comparably to the categorical estimator in many
#' conditions, especially when the thresholds are roughly symmetric, and that
#' with fewer categories, or with strongly asymmetric thresholds, the
#' categorical estimator (polychoric correlations, WLSMV) is more accurate.
#' The type is a proposal: the grid shows each proposed item's category counts
#' and skew so the researcher can judge, and can switch any item to ordered.
#' Constant and non numeric inputs are labeled accordingly. The same rule
#' holds for a type a specification or a project gives (see
#' `sieve_option_rule()`), counting an item's response options rather than
#' only its observed values.
#'
#' @param x A vector.
#' @param max_ordered Largest number of distinct integer values still
#'   proposed as ordered categorical rather than continuous.
#' @return A single character string: one of `"binary"`, `"ordered"`,
#'   `"continuous"`, `"constant"`, or `"text"`.
#' @author Ken Kelley
#' @noRd
sieve_detect_type <- function(x, max_ordered = 4L) {
  if (!is.numeric(x) && !is.integer(x) && !is.factor(x)) {
    return("text")
  }
  v <- suppressWarnings(as.numeric(x))
  u <- sort(unique(v[!is.na(v)]))
  k <- length(u)
  if (k <= 1) {
    return("constant")
  }
  if (k == 2) {
    return("binary")
  }
  int_like <- all(abs(u - round(u)) < .Machine$double.eps^0.5)
  if (int_like && k <= max_ordered) {
    return("ordered")
  }
  "continuous"
}

#' The number of response options of an item
#'
#' The larger of the item's distinct observed values and, for an integer
#' valued item whose response bounds are declared, the number of integers
#' the bounds span (`hi - lo + 1`): a seven point item on which only four
#' points were chosen has seven response options. An item that is not
#' integer valued has as many options as distinct values.
#'
#' @param x The item's values.
#' @param lo,hi The declared response bounds, or `NA`.
#' @return A single integer (0 when no value is observed).
#' @author Ken Kelley
#' @noRd
sieve_response_options <- function(x, lo = NA_real_, hi = NA_real_) {
  v <- sieve_as_number(x)
  v <- v[is.finite(v)]
  k <- length(unique(v))
  lo <- suppressWarnings(as.numeric(lo))
  hi <- suppressWarnings(as.numeric(hi))
  whole <- function(z) length(z) == 1L && is.finite(z) && abs(z - round(z)) < .Machine$double.eps^0.5
  int_like <- k > 0L && all(abs(v - round(v)) < .Machine$double.eps^0.5)
  if (int_like && whole(lo) && whole(hi) && hi > lo) {
    k <- max(k, as.integer(round(hi - lo)) + 1L)
  }
  as.integer(k)
}

#' The response options of every item in the ledger
#'
#' `sieve_response_options()` for each item, with the bounds the ledger
#' records as declared (`rev_bounds == "declared"`); bounds read from an
#' item's observed range add nothing to its observed values.
#'
#' @param items The item data.
#' @param meta The item ledger.
#' @return A data frame with `item`, `type`, `options` (the number of
#'   response options) and `observed` (the distinct observed values), one
#'   row per ledger item that is in the item data.
#' @author Ken Kelley
#' @noRd
sieve_item_options <- function(items, meta) {
  its <- intersect(as.character(meta$item), names(items))
  at <- match(its, meta$item)
  declared <- if ("rev_bounds" %in% names(meta)) as.character(meta$rev_bounds)[at] %in% "declared" else rep(
    FALSE,
    length(at)
  )
  lo <- if ("rev_lo" %in% names(meta)) suppressWarnings(as.numeric(meta$rev_lo))[at] else rep(NA_real_, length(at))
  hi <- if ("rev_hi" %in% names(meta)) suppressWarnings(as.numeric(meta$rev_hi))[at] else rep(NA_real_, length(at))
  lo[!declared] <- NA_real_
  hi[!declared] <- NA_real_
  data.frame(
    item = its,
    type = as.character(meta$type)[at],
    options = vapply(seq_along(its), function(j) sieve_response_options(items[[its[j]]], lo[j], hi[j]), integer(1)),
    observed = vapply(its, function(it) sieve_response_options(items[[it]]), integer(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

#' The items typed ordered that have five or more response options
#'
#' The rule the Ingest tab applies at every way in when the checkbox in
#' step 1 is on: an item typed
#' ordered with `min_options` or more response options (see
#' `sieve_response_options()`) is read as continuous, the treatment
#' detection already proposes for five or more distinct values (see
#' `sieve_detect_type()` for what Rhemtulla, Brosseau-Liard and Savalei,
#' 2012, found). An item typed binary, an item with four or fewer options,
#' and an item observed with fewer than three distinct values (binary in
#' the data whatever its declared bounds) keep their type.
#'
#' The rule switches a proposed type only: an ordered type the researcher
#' chose (`type_source` is `"chosen"`, see `sieve_type_source()`) is kept and
#' returned in `kept`. The bulk button above the grid is the researcher's own
#' request and passes `respect_chosen = FALSE`.
#'
#' @param items The item data.
#' @param meta The item ledger.
#' @param candidates The items the rule may switch (the ones a
#'   specification types ordered, say), or `NULL` for every item.
#' @param min_options The fewest response options read as continuous.
#' @param respect_chosen Whether an ordered type the researcher chose is
#'   kept.
#' @return A list with `switched` (the items to read as continuous),
#'   `kept` (items the rule would switch whose ordered type the researcher
#'   chose, kept) and `options` (a named integer vector: the response
#'   options of each item typed ordered among the candidates).
#' @author Ken Kelley
#' @noRd
sieve_option_rule <- function(items, meta, candidates = NULL, min_options = 5L, respect_chosen = TRUE) {
  tab <- sieve_item_options(items, meta)
  tab <- tab[tab$type %in% "ordered", , drop = FALSE]
  if (!is.null(candidates)) {
    tab <- tab[tab$item %in% candidates, , drop = FALSE]
  }
  switch_it <- tab$options >= min_options & tab$observed >= 3L
  chosen <- sieve_type_source(meta)[match(tab$item, meta$item)] == "chosen"
  kept <- if (isTRUE(respect_chosen)) switch_it & chosen else rep(FALSE, length(switch_it))
  list(
    switched = tab$item[switch_it & !kept], kept = tab$item[kept],
    options = stats::setNames(tab$options, tab$item)
  )
}

#' Where each item's type came from
#'
#' The ledger column `type_source`: `"proposed"` for a type detected at load
#' or read from a specification or from description rows, `"chosen"` for a
#' type the researcher set (the Type cell, the bulk type buttons, Set the type
#' of, the SPSS set-to-ordered button, the button that keeps switched items
#' ordered, a specification that says so, or `sieve_prepare_study()` with
#' `continuous_from = NULL`). A ledger without the column (a saved study
#' that does not record where its types came from) reads as `"proposed"`.
#'
#' @param meta The item ledger.
#' @return A character vector, one per ledger row, each `"proposed"` or
#'   `"chosen"`.
#' @author Ken Kelley
#' @noRd
sieve_type_source <- function(meta) {
  n <- NROW(meta)
  if (!n) {
    return(character(0))
  }
  src <- if ("type_source" %in% names(meta)) as.character(meta$type_source) else rep(NA_character_, n)
  ifelse(src %in% "chosen", "chosen", "proposed")
}

#' Record where some items' types came from
#'
#' @param meta The item ledger.
#' @param items Item names.
#' @param source `"proposed"` or `"chosen"`.
#' @return The ledger, with a `type_source` column (created after `type`
#'   when absent, every other item proposed).
#' @author Ken Kelley
#' @noRd
sieve_mark_type_source <- function(meta, items, source = c("chosen", "proposed")) {
  source <- match.arg(source)
  if (is.null(meta) || !NROW(meta)) {
    return(meta)
  }
  if (!"type_source" %in% names(meta)) {
    meta$type_source <- sieve_type_source(meta)
    at_type <- match("type", names(meta))
    if (!is.na(at_type)) {
      ordered <- append(setdiff(names(meta), "type_source"), "type_source", after = at_type)
      meta <- meta[, ordered, drop = FALSE]
    }
  } else {
    meta$type_source <- sieve_type_source(meta)
  }
  meta$type_source[meta$item %in% items] <- source
  meta
}

#' What a proposed type rests on, for the item grid
#'
#' The response options, the distinct values observed, the skew, and the
#' count of each category of an integer valued item with at most eleven
#' options, so the researcher can judge a proposed type against the
#' conditional finding of Rhemtulla, Brosseau-Liard and Savalei (2012).
#'
#' @param x The item's values.
#' @param lo,hi Declared response bounds, or `NA`.
#' @return A list with `options`, `observed`, `skew` (the moment
#'   coefficient of skewness, `NA` with fewer than three values or no
#'   variance), `counts` (a named integer vector of category counts, or
#'   `NULL`), and `text` (one short phrase).
#' @author Ken Kelley
#' @noRd
sieve_type_evidence <- function(x, lo = NA_real_, hi = NA_real_) {
  v <- sieve_as_number(x)
  v <- v[is.finite(v)]
  options <- sieve_response_options(v, lo, hi)
  observed <- length(unique(v))
  skew <- if (length(v) >= 3L && stats::sd(v) > 0) {
    m <- mean(v)
    mean((v - m)^3) / mean((v - m)^2)^1.5
  } else {
    NA_real_
  }
  int_like <- length(v) > 0L && all(abs(v - round(v)) < .Machine$double.eps^0.5)
  counts <- if (int_like && options <= 11L) {
    lo_c <- suppressWarnings(as.numeric(lo))
    hi_c <- suppressWarnings(as.numeric(hi))
    span <- if (is.finite(lo_c) && is.finite(hi_c) && options == hi_c - lo_c + 1) seq(lo_c, hi_c) else sort(unique(v))
    tab <- table(factor(v, levels = span))
    stats::setNames(as.integer(tab), names(tab))
  }
  text <- paste0(
    sprintf("%d option%s", options, if (options == 1L) "" else "s"),
    if (observed < options) sprintf(" (%d observed)", observed) else "",
    if (is.finite(skew)) sprintf(", skew %.2f", skew) else ""
  )
  list(options = options, observed = observed, skew = skew, counts = counts, text = text)
}

#' The five-option rule's switch in one sentence
#'
#' @param switched The items read as continuous.
#' @param source Where their ordered type came from, such as `"the
#'   specification"` or `"the project"`.
#' @return One sentence, or `NULL` when nothing was switched.
#' @author Ken Kelley
#' @noRd
sieve_option_rule_text <- function(switched, source = "the specification") {
  n <- length(switched)
  if (!n) {
    return(NULL)
  }
  sprintf(
    paste(
      "%s typed ordered in %s %s five or more response options and %s read as continuous: with five or",
      "more categories, estimation that treats items as continuous performs comparably to the categorical",
      "estimator in many conditions, especially with roughly symmetric thresholds, while with fewer",
      "categories or strongly asymmetric thresholds the categorical estimator is more accurate (Rhemtulla,",
      "Brosseau-Liard & Savalei, 2012); the Type cells show each item's options and skew. To keep the",
      "categorical treatment, set Type to ordered in the grid, or turn off the five-option checkbox in",
      "step 1 before loading."
    ),
    if (n == 1L) sprintf("1 item (%s)", switched) else sprintf("%d items (%s)", n, sieve_name_some(switched)),
    source, if (n == 1L) "has" else "have", if (n == 1L) "was" else "were"
  )
}



#' The study's estimator setting in words
#'
#' @param choice One of `sieve_estimator_choices()`; anything else is read
#'   as `"auto"`.
#' @return The label the Estimator control shows for the choice, such as
#'   `"ML with FIML (items treated as continuous)"`.
#' @author Ken Kelley
#' @noRd
sieve_estimator_choice_text <- function(choice) {
  own <- sieve_estimator_select_choices(study = FALSE)
  names(own)[match(sieve_estimator_effective(choice), own)]
}

#' Whether each construct cell names a construct
#'
#' The one rule for an item without a construct, used wherever a construct
#' counts as assigned (the model builders, the page gate, reliability,
#' scoring, norms and the study summaries). A construct cell that is
#' missing, blank (empty or only white space), or `"Unassigned"` names no
#' construct. `"Unassigned"` is the label SIEVER itself writes for such an
#' item (on Load, in a cleared cell, on Clear constructs, on a specification
#' import, and in the exported specification), so it is read as no
#' construct rather than refused as a name: refusing it would reject
#' SIEVER's own exported specifications and the Ingest dropdown's way of
#' unassigning an item.
#'
#' @param x A vector of construct cells (character or factor).
#' @return A logical vector the length of `x`, `TRUE` where the cell names a
#'   construct; never `NA`.
#' @author Ken Kelley
#' @noRd
sieve_construct_assigned <- function(x) {
  x <- as.character(x)
  !is.na(x) & nzchar(trimws(x)) & x != "Unassigned"
}

#' Build the item ledger from a loaded data frame
#'
#' Constructs the `meta` table that the winnowing engine reads and writes.
#' Each row is one item, carrying its presumed construct and facet, the
#' detected and (user editable) confirmed type, distinct value and missing
#' counts, an optional wording label, and a status of "active" or "parked".
#'
#' @param df The loaded data frame.
#' @param item_cols Character vector of item column names.
#' @param theory_map Optional named list mapping a construct name to its
#'   item columns (the seeded a priori structure).
#' @param labels Optional named character vector of item wordings, named by
#'   item column.
#' @return A data frame, one row per item; with a `spss_measure` column
#'   (Nominal, Ordinal, Scale) when the columns carry their SPSS measurement
#'   level (see `sieve_spss_mark()`).
#'
#' An item whose column carries value labels (an SPSS or Stata file read by
#' haven) has the response options the labels declare: their lowest and
#' highest value, user-missing codes left out (see `sieve_label_bounds()`),
#' are recorded as its declared response bounds when every observed value lies
#' within them, so a seven point item on which four points were chosen has
#' seven response options for the five-option rule, the bulk type buttons and
#' a reversal. Its proposed type follows the rule on those options: an integer
#' item observed on three or more values with five or more declared options is
#' proposed continuous; an item observed on two values stays binary (the data
#' hold two categories whatever the labels declare).
#' @author Ken Kelley
#' @noRd
sieve_build_meta <- function(df, item_cols, theory_map = NULL, labels = NULL) {
  construct <- rep("Unassigned", length(item_cols))
  facet <- rep(NA_character_, length(item_cols))
  if (!is.null(theory_map)) {
    for (cname in names(theory_map)) {
      grp <- theory_map[[cname]]
      its <- if (is.list(grp)) unlist(grp) else grp
      construct[item_cols %in% its] <- cname
    }
  }
  detected <- vapply(df[item_cols], sieve_detect_type, character(1))
  ndist <- vapply(df[item_cols], function(x) length(unique(x[!is.na(x)])), integer(1))
  # The response bounds the value labels declare, and the type they imply.
  bounds <- lapply(df[item_cols], sieve_label_bounds)
  declared_lo <- vapply(bounds, function(b) b[1], numeric(1))
  declared_hi <- vapply(bounds, function(b) b[2], numeric(1))
  by_labels <- is.finite(declared_lo) & is.finite(declared_hi)
  for (j in which(by_labels & detected == "ordered")) {
    options <- sieve_response_options(df[[item_cols[j]]], declared_lo[j], declared_hi[j])
    if (options >= 5L && ndist[j] >= 3L) detected[j] <- "continuous"
  }
  num_mean <- vapply(df[item_cols], function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
  }, numeric(1))
  num_sd <- vapply(df[item_cols], function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (sum(!is.na(v)) < 2L) NA_real_ else stats::sd(v, na.rm = TRUE)
  }, numeric(1))
  num_min <- vapply(df[item_cols], function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE)
  }, numeric(1))
  num_max <- vapply(df[item_cols], function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
  }, numeric(1))
  meta <- data.frame(
    item = item_cols,
    label = if (!is.null(labels)) labels[item_cols] else item_cols,
    # wording: the full supplied item text (SPSS variable label, fixture
    # description, or a supplied column), preserved read-only so that renaming
    # the editable Item Name never loses the original wording. NA when none is
    # supplied. Shown in the grids and detail panes to aid construct grouping.
    wording = if (!is.null(labels)) unname(labels[item_cols]) else NA_character_,
    construct = construct,
    facet = facet,
    detected = detected,
    type = detected,
    # Where the type came from: detected here, so proposed (see sieve_type_source()).
    type_source = "proposed",
    n_levels = ndist,
    n_missing = vapply(df[item_cols], function(x) sum(is.na(x)), integer(1)),
    mean = num_mean,
    sd = num_sd,
    min = num_min,
    max = num_max,
    reverse = FALSE,
    # The response bounds a reversal of the item reflects within, and where
    # they came from ("declared" or "observed"); see sieve_reverse_item().
    rev_lo = unname(ifelse(by_labels, declared_lo, NA_real_)),
    rev_hi = unname(ifelse(by_labels, declared_hi, NA_real_)),
    rev_bounds = unname(ifelse(by_labels, "declared", NA_character_)),
    status = "active",
    # Free-text annotations attached to an item by an action elsewhere in the
    # app (the reverse-coding action writes "reverse coded" here and clears it
    # on restore; the Content Validity tab writes an item's I-CVI). The column
    # exists from the start, so every action that annotates an item has it.
    note = NA_character_,
    stringsAsFactors = FALSE
  )
  # The SPSS measurement level of each item, when the data came from a .sav
  # file (see sieve_spss_mark()); the column exists only then.
  spss <- vapply(df[item_cols], function(x) {
    m <- attr(x, "spss_measure", exact = TRUE)
    if (is.null(m)) NA_character_ else as.character(m)[1]
  }, character(1))
  if (any(!is.na(spss))) {
    meta$spss_measure <- unname(spss)
  }
  meta
}

#' The response bounds an item's value labels declare
#'
#' The value labels haven keeps on a column read from an SPSS or Stata file
#' (`labels`) name the response options of the scale. Their lowest and
#' highest values are the item's bounds when they are whole numbers, after
#' leaving out the user-missing codes the SPSS dictionary declares
#' (`spss_missing`, see `sieve_spss_mark()`), and every observed value lies
#' within them; otherwise the labels declare nothing usable.
#'
#' @param x A column.
#' @return `c(lo, hi)`, or `c(NA, NA)`.
#' @author Ken Kelley
#' @noRd
sieve_label_bounds <- function(x) {
  none <- c(NA_real_, NA_real_)
  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs) || !is.numeric(unclass(labs))) {
    return(none)
  }
  v <- as.numeric(unclass(labs))
  missing_codes <- attr(x, "spss_missing", exact = TRUE)
  if (length(missing_codes)) {
    v <- v[!v %in% as.numeric(missing_codes$values)]
    rng <- as.numeric(missing_codes$range)
    if (length(rng) == 2L && all(is.finite(rng))) v <- v[v < rng[1] | v > rng[2]]
  }
  v <- v[is.finite(v)]
  if (length(v) < 2L || any(abs(v - round(v)) > .Machine$double.eps^0.5)) {
    return(none)
  }
  lo <- min(v)
  hi <- max(v)
  seen <- sieve_as_number(x)
  seen <- seen[is.finite(seen)]
  if (lo >= hi || (length(seen) && (min(seen) < lo || max(seen) > hi))) {
    return(none)
  }
  c(lo, hi)
}

#' The observed ranges of the items against the study's common range
#'
#' When no response bounds are declared, a reversal reflects an item within
#' its observed range (see `sieve_reverse_item()`). That is wrong for an item
#' whose observed range is narrower or wider than the scale it was
#' administered on, which shows when its range differs from the range most
#' items share. This finds the common range (the modal observed minimum and
#' maximum among the included items) and names the items whose observed range
#' differs and for which no bounds are declared.
#'
#' @param meta The item ledger.
#' @return `NULL` when fewer than two included items have an observed
#'   range; otherwise a list with `common` (`c(lo, hi)`, the modal range),
#'   `n_common` (how many included items share it), `n` (the included items
#'   with a range), `majority` (`TRUE` when more than half share it),
#'   `ranges` (a data frame of the distinct ranges with `lo`, `hi`, `n`,
#'   most common first), `differing` (a data frame with `item`, `label`,
#'   `min`, `max` of the items whose range differs and have no declared
#'   bounds, and `outside`, `TRUE` for such an item whose observed range lies
#'   within none of the bounds declared for other items, so that a
#'   declaration made for the study left it out; no rows when there are
#'   none), `declared` (the items with declared bounds, which the check
#'   leaves alone), and `bounds` (the distinct declared bounds, a data frame
#'   with `lo` and `hi`).
#' @author Ken Kelley
#' @noRd
sieve_range_check <- function(meta) {
  if (is.null(meta) || !nrow(meta)) {
    return(NULL)
  }
  active <- meta[meta$status == "active", , drop = FALSE]
  lo <- suppressWarnings(as.numeric(active$min))
  hi <- suppressWarnings(as.numeric(active$max))
  has <- is.finite(lo) & is.finite(hi)
  if (sum(has) < 2L) {
    return(NULL)
  }
  active <- active[has, , drop = FALSE]
  lo <- lo[has]
  hi <- hi[has]
  key <- paste(sieve_num_text(lo), sieve_num_text(hi), sep = "\r")
  counts <- sort(table(key), decreasing = TRUE)
  first <- which(key == names(counts)[1])[1]
  common <- c(lo[first], hi[first])
  ranges <- do.call(rbind, lapply(names(counts), function(k) {
    at <- which(key == k)[1]
    data.frame(lo = lo[at], hi = hi[at], n = as.integer(counts[[k]]), stringsAsFactors = FALSE)
  }))
  rownames(ranges) <- NULL
  declared <- as.character(active$item)[as.character(active$rev_bounds) %in% "declared" &
    is.finite(suppressWarnings(as.numeric(active$rev_lo))) &
    is.finite(suppressWarnings(as.numeric(active$rev_hi)))]
  differs <- key != names(counts)[1] & !active$item %in% declared
  # Whether each item's observed range lies within some declared bounds: an
  # item outside every declaration was left out of a declaration made for
  # the study; one inside has simply not been declared (a declaration in
  # one item's own cell says nothing about the others).
  dec <- as.character(active$item) %in% declared
  d_lo <- suppressWarnings(as.numeric(active$rev_lo))[dec]
  d_hi <- suppressWarnings(as.numeric(active$rev_hi))[dec]
  covered <- vapply(seq_along(lo), function(j) any(d_lo <= lo[j] & hi[j] <= d_hi), logical(1))
  differing <- data.frame(
    item = as.character(active$item)[differs],
    label = as.character(active$label)[differs],
    min = lo[differs], max = hi[differs],
    outside = length(declared) > 0L & !covered[differs],
    stringsAsFactors = FALSE
  )
  rownames(differing) <- NULL
  bounds <- unique(data.frame(lo = d_lo, hi = d_hi))
  rownames(bounds) <- NULL
  list(
    common = common, n_common = as.integer(counts[[1]]), n = length(key),
    majority = counts[[1]] > length(key) / 2, ranges = ranges, differing = differing,
    declared = declared, bounds = bounds
  )
}

#' The range warning in one sentence
#'
#' @param check A `sieve_range_check()` result.
#' @return One sentence naming the items whose observed range differs from
#'   the study's common range, or `NULL` when none does.
#' @author Ken Kelley
#' @noRd
sieve_range_text <- function(check) {
  if (is.null(check) || !nrow(check$differing)) {
    return(NULL)
  }
  d <- check$differing
  outside <- if ("outside" %in% names(d)) d$outside %in% TRUE else rep(length(check$declared) > 0L, nrow(d))
  named <- function(x) {
    paste(sprintf("%s (%s to %s)", x$item, sieve_num_text(x$min), sieve_num_text(x$max)), collapse = ", ")
  }
  out <- d[outside, , drop = FALSE]
  rest <- d[!outside, , drop = FALSE]
  n_declared <- length(check$declared)
  parts <- character(0)
  if (nrow(out)) {
    # A declaration made for the study left these items out: their values
    # lie outside it, so they are on a scale of their own.
    parts <- c(parts, sprintf(
      paste(
        "Bounds are declared for %d of the %d included items, and %d %s not covered because %s observed",
        "range lies outside them: %s. Such an item is on a scale of its own: declare its bounds in",
        "its Response bounds cell, or in a specification (rev_lo, rev_hi), or set its Reverse?",
        "knowing that it reflects within its observed range."
      ),
      n_declared, check$n, nrow(out), if (nrow(out) == 1L) "is" else "are",
      if (nrow(out) == 1L) "its" else "their", named(out)
    ))
  }
  if (nrow(rest)) {
    one <- nrow(rest) == 1L
    # The lead says how the items without declared bounds are reflected,
    # once: after the sentence on the items outside the declared bounds it
    # does not repeat the count of the declared ones.
    lead <- if (!n_declared) {
      "No response bounds are declared, so a reversal reflects an item within its observed range."
    } else if (nrow(out)) {
      "A reversal also reflects each item without declared bounds within its observed range."
    } else if (isTRUE(check$majority)) {
      sprintf(
        paste(
          "Bounds are declared for %d of the %d included items but not for the %s named here, so a",
          "reversal reflects %s within its observed range."
        ),
        n_declared, check$n, if (one) "item" else "items", if (one) "it" else "each of them"
      )
    } else {
      n_other <- check$n - n_declared
      sprintf(
        "Bounds are declared for %d of the %d included items, so a reversal reflects %s within its observed range.",
        n_declared, check$n, if (n_other == 1L) "the other" else sprintf("each of the other %d", n_other)
      )
    }
    subject <- if (!n_declared) {
      as.character(nrow(rest))
    } else {
      sprintf("%d %sitem%s without declared bounds", nrow(rest), if (nrow(out)) "other " else "", if (one) "" else "s")
    }
    parts <- c(parts, if (isTRUE(check$majority)) {
      sprintf(
        paste(
          "%s %d of the %d included items span %s to %s, but %s %s a different observed range: %s.",
          "Reflecting such an item within its own range is wrong if it was administered on the",
          "same scale as the others; declare the bounds of every item."
        ),
        lead, check$n_common, check$n, sieve_num_text(check$common[1]), sieve_num_text(check$common[2]),
        subject, if (one) "has" else "have", named(rest)
      )
    } else {
      spread <- paste(sprintf(
        "%s to %s for %d item%s", sieve_num_text(check$ranges$lo),
        sieve_num_text(check$ranges$hi), check$ranges$n,
        ifelse(check$ranges$n == 1L, "", "s")
      ), collapse = ", ")
      if (n_declared) {
        sprintf(paste(
          "%s The observed ranges differ across the included items, with no range shared by a",
          "majority (%s); without declared bounds and outside the most common range: %s. An item",
          "whose observed range is not the scale it was administered on would be reflected wrongly;",
          "declare the bounds of every item."
        ), lead, spread, named(rest))
      } else {
        sprintf(paste(
          "No response bounds are declared, so a reversal reflects an item within its observed range,",
          "and the observed ranges differ across the included items with no range shared by a",
          "majority (%s). An item whose observed range is not the scale it was administered on",
          "would be reflected wrongly; declare the bounds of every item."
        ), spread)
      }
    })
  }
  paste(parts, collapse = " ")
}

#' The roles a variable can have
#'
#' A role says what a column of the data is for, independently of how it is
#' stored: an `item` belongs to the item pool, an `identifier` names the rows,
#' a `group` defines the groups of a multiple group analysis, a `criterion` is
#' an external variable the scores are related to, an `antecedent` is a
#' variable that exists before the target construct (a background or prior
#' variable the constructs may depend on; offered as an observed predictor on
#' the SEM tab, never as a criterion, and never proposed automatically), and
#' an `auxiliary` variable is kept with the data and used for none of these.
#'
#' @return A character vector of the six roles.
#' @author Ken Kelley
#' @noRd
sieve_role_levels <- function() {
  c("item", "identifier", "group", "criterion", "antecedent", "auxiliary")
}

#' The kinds of validity evidence a criterion can be for
#'
#' A criterion is an external variable the construct scores are related
#' to. Its kind says which prediction the Validity tab reads it against:
#' `convergent` (a related construct, where a sizeable correlation is
#' predicted), `discriminant` (a distinct construct, where a small one is
#' predicted), or `criterion` (criterion related validity: an outcome the
#' scores should predict). The kind is kept in the `kind` column of the
#' roles table and is empty when not stated.
#'
#' @return A character vector of the three kinds.
#' @author Ken Kelley
#' @noRd
sieve_role_kinds <- function() {
  c("convergent", "discriminant", "criterion")
}

#' The words of a variable name
#'
#' Splits a name at anything that is not a letter or a digit and where a
#' lower case letter meets a capital, lower cases the pieces and drops
#' trailing digits, so `Subject_ID`, `subjectId` and `subject.id2` all give
#' `subject` and `id`.
#'
#' @param name A single variable name.
#' @return A character vector of words (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_name_words <- function(name) {
  spaced <- gsub("([a-z])([A-Z])", "\\1 \\2", as.character(name))
  words <- strsplit(tolower(spaced), "[^a-z0-9]+")[[1]]
  words <- sub("[0-9]+$", "", words)
  words[nzchar(words)]
}

#' Propose a role for every column of a data frame
#'
#' The proposal the researcher reviews after a file is read. It is never
#' more than a proposal: nothing here decides a role, and every role can be
#' changed before the item types are confirmed. The rules, in the order
#' they are tried:
#'
#' * A column that is not stored as a number cannot be an item. It is
#'   proposed as an identifier when every value is different, as a group
#'   when it has 2 to `max_groups` distinct values, and as auxiliary
#'   otherwise.
#' * A numeric column is proposed as an identifier when every value is
#'   different and either its name says so (`id`, `subject_id`, `case`),
#'   its values are consecutive whole numbers (it numbers the rows), or
#'   its values are whole numbers in at least `min_coded` rows (a code
#'   that names each row, such as a participant number).
#' * A numeric column whose name is that of a grouping variable (`sex`,
#'   `gender`, `group`, `school`, `grade`, `condition`) is proposed as a
#'   group when it has 2 to `max_groups` distinct values, and as auxiliary
#'   when it has more.
#' * A numeric column whose name is that of an outcome (`criterion`,
#'   `outcome`, `gpa`) is proposed as a criterion, and one whose name is
#'   that of a background variable (`age`, `income`, `tenure`) as auxiliary.
#' * Every other numeric column is proposed as an item.
#'
#' A name counts only when all of its words are role words (or neutral
#' words such as `id`, `code`, `level`), so `school` and `school_id` are
#' read as grouping variables and `school_belonging_3` is read as an item.
#' And a name does not count when it is one of a numbered series: a name
#' that ends in a number whose stem another numbered column shares
#' (`cond1`, `cond2`, `cond3`; `time_1`, `time_2`) is how the items of a
#' scale are named, so such columns are proposed as items whatever the
#' stem says; `class_3` on its own is still read by its name.
#'
#' @param df The loaded data frame.
#' @param max_groups The largest number of distinct values proposed as a
#'   group.
#' @param min_coded The smallest number of rows at which whole numbers that
#'   are all different are read as a code that names the rows.
#' @return A data frame with one row per column of `df`: `variable`, `role`
#'   (the current role, here the proposed one), `proposed`, `reason` (why,
#'   in plain words), `numeric` (whether the column is stored as a number
#'   and so can be an item), `n_distinct`, `n_missing`, and `kind` (the
#'   validity kind of a criterion, see `sieve_role_kinds()`; `NA` here,
#'   since nothing proposes one).
#' @author Ken Kelley
#' @noRd
sieve_propose_roles <- function(df, max_groups = 6L, min_coded = 10L) {
  id_words <- c(
    "id", "ids", "identifier", "caseid", "subjid", "pid", "uid", "uuid",
    "respid", "recordid", "responseid", "rownum", "serial", "key"
  )
  unit_words <- c(
    "case", "subject", "subj", "participant", "respondent", "record", "row",
    "obs", "observation", "student", "person"
  )
  group_words <- c(
    "sex", "gender", "group", "grp", "school", "grade", "condition", "cond",
    "treatment", "treat", "arm", "cohort", "class", "classroom", "site",
    "country", "region", "race", "ethnicity", "ethnic", "wave", "sample",
    "cluster", "female", "male"
  )
  criterion_words <- c("criterion", "crit", "outcome", "dv", "gpa")
  background_words <- c(
    "age", "ageyr", "agemo", "years", "yrs", "year", "months", "income",
    "salary", "education", "educ", "tenure", "height", "weight", "bmi",
    "date", "time", "duration", "seconds", "minutes", "timestamp",
    "birth", "birthyear", "yob", "dob", "zip", "zipcode", "postal"
  )
  neutral_words <- c(
    "id", "code", "cd", "num", "number", "no", "nr", "level", "lvl", "cat",
    "category", "var", "status", "type", "name"
  )
  named <- function(words, set) {
    length(words) > 0L && any(words %in% set) && all(words %in% c(set, neutral_words))
  }
  # The columns that are one of a numbered series.
  stem_of <- function(name) tolower(sub("[^A-Za-z0-9]*[0-9]+$", "", name))
  numbered <- grepl("[0-9]$", names(df))
  stems <- stem_of(names(df))
  in_series <- numbered & nzchar(stems) & stems %in% stems[numbered][duplicated(stems[numbered])]
  names(in_series) <- names(df)
  one <- function(name) {
    x <- df[[name]]
    seen <- x[!is.na(x)]
    k <- length(unique(seen))
    all_differ <- length(seen) >= 2L && k == length(seen)
    words <- sieve_name_words(name)
    numeric <- is.numeric(x)
    if (!numeric) {
      if (all_differ) {
        return(list("identifier", "it is not stored as a number and every value is different"))
      }
      if (k >= 2L && k <= max_groups) {
        return(list("group", sprintf("it is not stored as a number and has %d distinct values", k)))
      }
      return(list("auxiliary", "it is not stored as a number, so it cannot be an item"))
    }
    v <- as.numeric(unclass(seen))
    whole <- length(v) > 0L && all(v == trunc(v))
    if (all_differ && (any(words %in% id_words) || named(words, unit_words))) {
      return(list("identifier", "its name is that of an identifier and every value is different"))
    }
    if (all_differ && whole && length(v) >= 3L && max(v) - min(v) == length(v) - 1L) {
      return(list(
        "identifier",
        "its values are consecutive whole numbers, every one different (it numbers the rows)"
      ))
    }
    if (all_differ && whole && length(v) >= min_coded) {
      return(list(
        "identifier",
        "its values are whole numbers, every one different (a code that names the rows)"
      ))
    }
    if (in_series[[name]]) {
      return(list("item", sprintf(
        "it is one of the numbered series %s, which is how the items of a scale are named",
        paste0(stem_of(name), "...")
      )))
    }
    if (named(words, group_words)) {
      if (k >= 2L && k <= max_groups) {
        return(list("group", sprintf(
          "its name is that of a grouping variable and it has %d distinct values", k
        )))
      }
      return(list("auxiliary", sprintf(
        "its name is that of a grouping variable, but it has %d distinct value%s",
        k, if (k == 1L) "" else "s"
      )))
    }
    if (named(words, criterion_words)) {
      return(list("criterion", "its name is that of an outcome, not of a scale item"))
    }
    if (named(words, background_words)) {
      return(list("auxiliary", "its name is that of a background variable, not of a scale item"))
    }
    list("item", "it is stored as a number and nothing marks it as anything else")
  }
  vars <- names(df)
  found <- lapply(vars, one)
  role <- vapply(found, function(f) f[[1]], character(1))
  data.frame(
    variable = vars,
    role = role,
    proposed = role,
    reason = vapply(found, function(f) f[[2]], character(1)),
    numeric = vapply(vars, function(v) is.numeric(df[[v]]), logical(1), USE.NAMES = FALSE),
    n_distinct = vapply(vars, function(v) length(unique(df[[v]][!is.na(df[[v]])])), integer(1),
      USE.NAMES = FALSE
    ),
    n_missing = vapply(vars, function(v) sum(is.na(df[[v]])), integer(1), USE.NAMES = FALSE),
    kind = rep(NA_character_, length(vars)),
    stringsAsFactors = FALSE
  )
}

#' Find items that are the same item twice
#'
#' An item and its reversed copy correlate -1 exactly when both are in the
#' data: a reflection within bounds, `(min + max) - x`, is a linear function
#' of the item, and their sum is the same constant in every case. A file
#' carries both when the reversed version was computed and saved beside the
#' original (as `q3` and `q3_r`, say); once one of them is reverse scored
#' here, the pair correlates +1 instead. Two distinct items do not correlate
#' that strongly, so a pair of included items whose absolute correlation is
#' at least `threshold` is reported as one item twice: only one of the two
#' belongs in the analysis, which counts it twice otherwise (a duplicate
#' pair gives a residual correlation of one and a singular correlation
#' matrix). For a pair of opposite direction, the one to keep is the one
#' keyed with its construct: the item whose correlation with the mean of
#' the construct's other included items (every copy of the item left out) is
#' positive (the convention of a corrected item-total correlation). For a pair of the same direction the
#' item that was not reverse scored here is kept. When the direction cannot
#' be told (no construct, or fewer than two other included items in it),
#' no item is proposed.
#'
#' @param items The item data (one column per item).
#' @param meta The ledger (`item`, `status`, `construct`, and `reverse`
#'   when present).
#' @param threshold Smallest absolute correlation reported.
#' @param min_n Fewest cases with both items answered for a pair to be
#'   judged.
#' @return A data frame with one row per pair: `a`, `b`, `r` (their
#'   correlation, full precision), `n`, `constant_sum` (whether `a + b` is
#'   the same in every case, the signature of a reflection), `sum` (that
#'   constant, or `NA`), `r_a` and `r_b` (each item's correlation with the
#'   mean of its construct's other included items, or `NA`), `keep` and
#'   `drop` (the proposal, or `NA` when it cannot be told). No rows when
#'   there is no such pair.
#' @author Ken Kelley
#' @noRd
sieve_item_copies <- function(items, meta, threshold = 0.98, min_n = 10L) {
  empty <- data.frame(
    a = character(0), b = character(0), r = numeric(0), n = integer(0),
    constant_sum = logical(0), sum = numeric(0), r_a = numeric(0), r_b = numeric(0),
    keep = character(0), drop = character(0), stringsAsFactors = FALSE
  )
  if (is.null(items) || is.null(meta) || !nrow(meta)) {
    return(empty)
  }
  active <- as.character(meta$item[meta$status == "active"])
  active <- active[active %in% names(items)]
  active <- active[vapply(active, function(it) is.numeric(items[[it]]), logical(1))]
  if (length(active) < 2L) {
    return(empty)
  }
  x <- as.matrix(as.data.frame(lapply(items[active], as.numeric)))
  colnames(x) <- active
  ok <- !is.na(x)
  both <- crossprod(ok)
  R <- suppressWarnings(stats::cor(x, use = "pairwise.complete.obs"))
  hit <- which(upper.tri(R) & abs(R) >= threshold & both >= min_n, arr.ind = TRUE)
  if (!nrow(hit)) {
    return(empty)
  }
  construct_of <- stats::setNames(as.character(meta$construct), as.character(meta$item))
  reversed <- if ("reverse" %in% names(meta)) {
    stats::setNames(meta$reverse %in% TRUE, as.character(meta$item))
  } else {
    stats::setNames(rep(FALSE, nrow(meta)), as.character(meta$item))
  }
  # The copies of one item, as connected sets (an item saved three times is
  # one set), so that every copy is left out of the construct's mean below.
  group <- stats::setNames(seq_along(active), active)
  for (k in seq_len(nrow(hit))) {
    from <- group[[hit[k, 2L]]]
    group[group == from] <- group[[hit[k, 1L]]]
  }
  # Each item's correlation with the mean of its construct's other included
  # items, leaving out every copy of the item.
  with_rest <- function(it, pair) {
    cn <- construct_of[[it]]
    if (!sieve_construct_assigned(cn)) {
      return(NA_real_)
    }
    copies_of <- active[group %in% group[pair]]
    rest <- setdiff(active[construct_of[active] %in% cn], copies_of)
    if (length(rest) < 2L) {
      return(NA_real_)
    }
    m <- rowMeans(x[, rest, drop = FALSE], na.rm = TRUE)
    m[!is.finite(m)] <- NA_real_
    suppressWarnings(stats::cor(x[, it], m, use = "pairwise.complete.obs"))
  }
  rows <- lapply(seq_len(nrow(hit)), function(k) {
    a <- active[hit[k, 1L]]
    b <- active[hit[k, 2L]]
    r <- R[a, b]
    keep_rows <- ok[, a] & ok[, b]
    s <- x[keep_rows, a] + x[keep_rows, b]
    constant <- r < 0 && length(s) > 0L && (max(s) - min(s)) <= 1e-8 * max(1, abs(s))
    r_a <- with_rest(a, c(a, b))
    r_b <- with_rest(b, c(a, b))
    keep <- NA_character_
    if (r < 0) {
      if (is.finite(r_a) && is.finite(r_b) && sign(r_a) != sign(r_b)) {
        keep <- if (r_a > 0) a else b
      }
    } else {
      keep <- if (reversed[[b]] && !reversed[[a]]) a else if (reversed[[a]] && !reversed[[b]]) b else a
    }
    data.frame(
      a = a, b = b, r = r, n = as.integer(both[a, b]),
      constant_sum = isTRUE(constant), sum = if (isTRUE(constant)) s[[1L]] else NA_real_,
      r_a = r_a, r_b = r_b, keep = keep,
      drop = if (is.na(keep)) NA_character_ else setdiff(c(a, b), keep),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(-abs(out$r), out$a, out$b), , drop = FALSE]
}

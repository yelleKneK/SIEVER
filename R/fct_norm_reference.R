#' Exact scoring definition for norm references
#' @param meta Item ledger.
#' @param policies Per-construct scoring rules.
#' @return Named definitions, independent of the current cohort.
#' @author Ken Kelley
#' @noRd
sieve_norm_definition <- function(meta, policies = list()) {
  active <- meta[meta$status == "active", , drop = FALSE]
  cons <- unique(as.character(active$construct))
  cons <- cons[sieve_construct_assigned(cons)]
  stats::setNames(lapply(cons, function(cn) {
    m <- active[active$construct == cn & !is.na(active$construct), , drop = FALSE]
    keep <- intersect(
      c(
        "item",
        "construct",
        "type",
        "reverse",
        "rev_lo",
        "rev_hi",
        "rev_bounds",
        "version",
        "wording",
        "question"
      ),
      names(m)
    )
    m <- m[, keep, drop = FALSE]
    rownames(m) <- NULL
    # The policy as the score specification reads it (a policy that names
    # no method is the mean of the answered items).
    p <- sieve_score_policy_read(policies[[cn]], m$item)
    w <- p$weights %||% stats::setNames(rep(1, nrow(m)), m$item)
    if (!is.null(names(w))) w <- w[m$item]
    # A stored policy repaired on reading keeps its stored method and the
    # repair beside the definition in force; sieve_norm_definition_read()
    # compares definitions without them.
    list(
      items = as.character(m$item), metadata = m, method = p$method, min_count = p$min_count,
      min_prop = p$min_prop, weights = stats::setNames(as.numeric(w), m$item),
      score = switch(p$method,
        mean = "available-response weighted mean",
        sum = "available-response weighted mean prorated to the sum over every item",
        "factor score from the fitted measurement model, not a composite"
      ),
      input = "processed, reversals already applied",
      requested_method = p$requested, review = p$issue
    )
  }), cons)
}

#' A norm definition as it is compared
#'
#' Every definition is read in one form: one that names no `method` is the
#' mean of the answered items, its minimum count is an integer however it
#' was stored (1 when none is stored), and its minimum proportion is 0 when
#' none is stored. A frozen reference stored in either form therefore
#' compares equal to the current definition of the same items under the
#' same policy.
#'
#' @param definition The `definition` of a frozen reference or of
#'   `sieve_norm_definition()`.
#' @return The definition with `method`, `min_count`, and `min_prop`
#'   normalized and its elements in one order.
#' @author Ken Kelley
#' @noRd
sieve_norm_definition_read <- function(definition) {
  lapply(definition, function(d) {
    d$method <- d$method %||% "mean"
    d$min_count <- as.integer(d$min_count %||% 1L)
    d$min_prop <- as.numeric(d$min_prop %||% 0)
    d[c("items", "metadata", "method", "min_count", "min_prop", "weights", "score", "input")]
  })
}

#' Percentile uncertainty without jittering tied scores
#' @param x Independent eligible scores.
#' @param probs Percentiles on the probability scale.
#' @param level Confidence level.
#' @return Limits, tail counts, and the stated method.
#' @author Ken Kelley
#' @noRd
sieve_quantile_limits <- function(x, probs = c(.05, .5, .95), level = .95) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (!n || any(!is.finite(probs) | probs <= 0 | probs >= 1) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) stop("Supply finite scores, interior probabilities, and a confidence level in (0, 1).",
    call. = FALSE
  )
  alpha <- 1 - level
  # Binomial order-statistic limits cover the generalized-inverse population
  # quantile for any distribution. Atoms make coverage conservative, not
  # invalid; one tie must not silently change a pointwise interval into a
  # simultaneous confidence band.
  lo <- stats::qbinom(alpha / 2, n, probs)
  hi <- stats::qbinom(1 - alpha / 2, n, probs) + 1L
  lower <- vapply(lo, function(k) if (k < 1) -Inf else x[k], numeric(1))
  upper <- vapply(hi, function(k) if (k > n) Inf else x[k], numeric(1))
  method <- "pointwise distribution-free binomial order-statistic limits; conservative with ties"
  data.frame(
    probability = probs, estimate = as.numeric(stats::quantile(x, probs, type = 7)),
    lower = lower, upper = upper, level = level, n = n,
    expected_lower_tail = n * probs, expected_upper_tail = n * (1 - probs),
    distinct_scores = length(unique(x)), method = method, stringsAsFactors = FALSE
  )
}

#' Freeze Calibration Norms With Their Scoring Definition and Target Population
#'
#' @description
#' Fixes the norms of a calibration sample in one self-contained object:
#' each construct's scoring definition (items, reversals, weights and
#' missing-response rule), its calibration mean and standard deviation, the
#' calibration scores behind the percentile ranks, and the target population
#' and sampling design the norms describe. A frozen reference can be saved,
#' read back after a plain-data check, and applied to new respondents with
#' [sieve_norm_apply()].
#'
#' @param data Processed calibration items.
#' @param meta Item ledger.
#' @param policies Per-construct scoring rules.
#' @param name,population Researcher-defined reference and target population.
#' @param rows Stable calibration row references.
#' @param calibration_scientific_dataset_id Optional persistent identity of the calibration sample.
#' @param design Sampling design declaration.
#' @param cluster Optional independent cluster labels aligned to all calibration rows.
#' @return A self-contained frozen reference, a list of class
#'   `siever_norm_reference`: `format`, `name`, `population`, `created`,
#'   `limitation`, `definition` (each construct's items, ledger entries,
#'   weights and missing-response rule), `table` (each construct's n, mean,
#'   SD and percentiles), `scores` (each construct's eligible calibration
#'   scores, sorted, which the empirical percentiles of
#'   [sieve_norm_apply()] need), `uncertainty`, `quantile_type`, `design`,
#'   `calibration_scientific_dataset_id`, `calibration_rows` and
#'   `eligible_rows` (the calibration row numbers, all and per construct),
#'   and `bootstrap_scores`. With `cluster` supplied, `bootstrap_scores`
#'   keeps for each construct every respondent's score with that
#'   respondent's row number and cluster label, in calibration order (whole
#'   clusters are resampled from them); otherwise it is `NULL`. A reference
#'   is therefore respondent-level data, and a clustered one links each
#'   score to its row and cluster: share a saved reference (Download frozen
#'   reference (`.rds`), or [saveRDS()]) only as the study's data-sharing
#'   arrangements allow.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' reference <- sieve_norm_freeze(items, meta, name = "Holzinger and Swineford calibration",
#'   population = "Grade 7 and 8 students in two Chicago schools, 1939")
#' reference$table
#' @export
sieve_norm_freeze <- function(
  data,
  meta,
  policies = list(),
  name,
  population,
  rows = seq_len(nrow(data)),
  design = list(
    unit = "independent",
    cluster = NULL
  ),
  calibration_scientific_dataset_id = NULL, cluster = NULL
) {
  if (length(name) != 1L || !nzchar(trimws(name)) || length(population) != 1L || !nzchar(trimws(population))) stop("Name the reference and describe its eligible population.",
    call. = FALSE
  )
  if (length(rows) != nrow(data) || anyNA(rows) || anyDuplicated(rows)) stop("Calibration rows must identify the supplied observations uniquely.",
    call. = FALSE
  )
  r <- sieve_construct_norms(data, meta, policies)
  if (is.null(r$table)) stop(paste(c("No eligible construct norms.", r$refused), collapse = " "), call. = FALSE)
  # A frozen reference scores later cohorts by its definition, so it is
  # never frozen on a policy that could not be used as saved.
  if (length(r$review)) {
    stop(sprintf(
      "A frozen reference keeps its scoring definition for scoring later cohorts, and the stored scoring policy of %s needs review: %s Keep a method for %s (Reliability tab, Operational scores) before freezing.",
      paste(names(r$review), collapse = ", "), paste(sprintf("%s: %s", names(r$review), r$review), collapse = " "),
      if (length(r$review) == 1L) "it" else "each"
    ), call. = FALSE)
  }
  if (!identical(design$unit %||% "independent", "independent")) {
    r$uncertainty$lower <- NA_real_
    r$uncertainty$upper <- NA_real_
    r$uncertainty$method <- "Unavailable: dependent calibration requires design-matched percentile inference"
  }
  if (identical(design$unit, "clustered") && !is.null(cluster)) sieve_bootstrap_partition(nrow(data), cluster)
  structure(
    list(
      bootstrap_scores = if (!is.null(cluster)) lapply(r$scores, function(v) {
        ok <- is.finite(v)
        list(score = v[ok], rows = rows[ok], cluster = cluster[ok])
      }),
      format = "SIEVER frozen norm reference 1", name = name, population = population,
      created = format(Sys.time(), tz = "UTC", usetz = TRUE), definition = r$definition,
      calibration_rows = rows, eligible_rows = lapply(r$scores, function(v) rows[is.finite(v)]),
      calibration_scientific_dataset_id = calibration_scientific_dataset_id,
      table = r$table, scores = lapply(r$scores, function(v) sort(v[is.finite(v)])),
      uncertainty = r$uncertainty, quantile_type = 7L, design = design,
      limitation = "Calibration-sample reference; population generalization requires representative calibration and a defensible sampling design."
    ),
    class = "siever_norm_reference"
  )
}

#' What keeps an object from being a usable frozen norm reference
#'
#' The structure [sieve_norm_apply()], [sieve_norm_transform_limits()] and
#' the Scoring and Norms page read, checked field by field on an object
#' already known to be plain data (`sieve_plain_unserialized()`), so that
#' a malformed reference is refused with the field named rather than with
#' an error from base R.
#'
#' @param reference The object.
#' @return The problems, each naming its field (empty when there are none).
#' @author Ken Kelley
#' @noRd
sieve_norm_reference_problems <- function(reference) {
  if (!is.list(reference) || !inherits(reference, "siever_norm_reference")) {
    return("it is not of class siever_norm_reference, the class sieve_norm_freeze() gives")
  }
  if (!identical(reference$format, "SIEVER frozen norm reference 1")) {
    return("reference$format is not \"SIEVER frozen norm reference 1\"")
  }
  string <- function(v) is.character(v) && length(v) == 1L && !is.na(v)
  optional_string <- function(v) is.null(v) || string(v)
  number <- function(v) is.numeric(v) && length(v) == 1L && !is.na(v)
  problems <- character(0)
  for (field in c("name", "population")) {
    if (!string(reference[[field]]) || !nzchar(trimws(reference[[field]]))) {
      problems <- c(problems, sprintf("reference$%s is not a single nonempty character string", field))
    }
  }
  for (field in c("created", "limitation", "calibration_scientific_dataset_id")) {
    if (!optional_string(reference[[field]])) {
      problems <- c(problems, sprintf("reference$%s is neither NULL nor a single character string", field))
    }
  }
  table <- reference$table
  if (!is.data.frame(table) || !all(c("construct", "mean", "sd") %in% names(table)) || !nrow(table)) {
    return(c(problems, "reference$table is not a data frame with a row per construct and the columns construct, mean and sd"))
  }
  constructs <- table$construct
  if (!is.character(constructs) || anyNA(constructs) || anyDuplicated(constructs)) {
    return(c(problems, "reference$table$construct does not name each construct once"))
  }
  if (!is.numeric(table$mean) || !is.numeric(table$sd)) {
    problems <- c(problems, "reference$table$mean or reference$table$sd is not numeric")
  }
  definition <- reference$definition
  if (!is.list(definition) || is.data.frame(definition) || !all(constructs %in% names(definition))) {
    problems <- c(problems, "reference$definition is not a list with a definition named for each construct of reference$table")
  } else {
    for (cn in constructs) {
      d <- definition[[cn]]
      here <- sprintf("reference$definition$%s", cn)
      if (!is.list(d) || is.data.frame(d)) {
        problems <- c(problems, sprintf("%s is not a list", here))
        next
      }
      if (!is.character(d$items) || !length(d$items) || anyNA(d$items)) {
        problems <- c(problems, sprintf("%s$items is not a character vector of item names", here))
      } else if (!is.numeric(d$weights) || length(d$weights) != length(d$items) || anyNA(d$weights)) {
        problems <- c(problems, sprintf("%s$weights is not a number for each of its %d items", here, length(d$items)))
      }
      if (!optional_string(d$method)) {
        problems <- c(problems, sprintf("%s$method is neither NULL nor a single character string", here))
      }
      for (rule in c("min_count", "min_prop")) {
        if (!is.null(d[[rule]]) && !number(d[[rule]])) {
          problems <- c(problems, sprintf("%s$%s is neither NULL nor a single number", here, rule))
        }
      }
    }
  }
  scores <- reference$scores
  if (!is.list(scores) || is.data.frame(scores) || !setequal(names(scores), constructs)) {
    problems <- c(problems, "reference$scores is not a list with the calibration scores named for each construct of reference$table")
  } else {
    for (cn in constructs) {
      v <- scores[[cn]]
      if (!is.numeric(v) || !length(v) || !all(is.finite(v))) {
        problems <- c(problems, sprintf("reference$scores$%s is not a nonempty vector of finite scores", cn))
      }
    }
  }
  design <- reference$design
  if (!is.null(design) && (!is.list(design) || is.data.frame(design) || !optional_string(design$unit))) {
    problems <- c(problems, "reference$design is neither NULL nor a list whose unit is a single character string")
  }
  held <- reference$bootstrap_scores
  if (!is.null(held)) {
    if (!is.list(held) || is.data.frame(held) || !all(constructs %in% names(held))) {
      problems <- c(problems, "reference$bootstrap_scores is neither NULL nor a list named for each construct of reference$table")
    } else {
      for (cn in constructs) {
        b <- held[[cn]]
        if (!is.list(b) || is.data.frame(b) || !is.numeric(b$score) || !is.numeric(b$rows) ||
          !is.atomic(b$cluster) || length(unique(c(length(b$score), length(b$rows), length(b$cluster)))) != 1L) {
          problems <- c(problems, sprintf(
            "reference$bootstrap_scores$%s is not a list of score, rows and cluster of one length", cn
          ))
        }
      }
    }
  }
  uncertainty <- reference$uncertainty
  if (!is.null(uncertainty) && !is.data.frame(uncertainty)) {
    problems <- c(problems, "reference$uncertainty is neither NULL nor a data frame")
  }
  problems
}

#' What a saved frozen reference holds, in words
#'
#' For the Scoring and Norms page, beside the reference that Download
#' frozen reference (`.rds`) writes.
#'
#' @param reference A frozen reference.
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_norm_reference_contents_text <- function(reference) {
  paste(
    "Download frozen reference (.rds) writes respondent-level data: each construct's calibration",
    "scores (sorted, which the empirical percentiles need) and the calibration row numbers.",
    if (!is.null(reference$bootstrap_scores)) {
      paste(
        "This reference is clustered, so it also keeps every respondent's score with that",
        "respondent's row number and cluster label, which link each score to a row and a cluster."
      )
    },
    "Share the file only as the study's data-sharing arrangements allow."
  )
}

#' Refuse an object that is not a plain, usable frozen norm reference
#'
#' Checks first that the object is plain data (`sieve_plain_unserialized()`,
#' which reads no field and so runs nothing), then its structure
#' (`sieve_norm_reference_problems()`).
#'
#' @param holder `list(<the object>)`.
#' @param file Whether the object was read from a file by
#'   [sieve_norm_read()] (the refusals then speak of the file and of
#'   importing it) or passed as an argument.
#' @return The reference, invisibly; otherwise stops, naming the place and
#'   the type or class found, or the field and what is wrong with it.
#' @author Ken Kelley
#' @noRd
sieve_norm_reference_check <- function(holder, file = FALSE) {
  bad <- sieve_plain_unserialized(holder, "reference")
  if (length(bad)) {
    stop(sprintf(
      if (file) {
        paste(
          "The file holds something that is not plain data (%s), so it was not imported.",
          "A frozen norm reference holds only data; nothing in the file was run."
        )
      } else {
        paste(
          "reference holds something that is not plain data (%s), so it was not used.",
          "A frozen norm reference holds only data; nothing in it was run."
        )
      },
      sieve_plain_problems_text(bad)
    ), call. = FALSE)
  }
  reference <- .subset2(holder, 1L)
  problems <- sieve_norm_reference_problems(reference)
  if (length(problems)) {
    stop(sprintf(
      if (file) {
        paste(
          "The file is not a frozen SIEVER norm reference that can be imported (%s), so nothing was",
          "imported. Import a reference that SIEVER's Download frozen reference (.rds) wrote, or that",
          "saveRDS() wrote from the value of sieve_norm_freeze()."
        )
      } else {
        paste(
          "reference is not a frozen SIEVER norm reference that can be used (%s). Freeze one with",
          "sieve_norm_freeze(), or read a saved one with sieve_norm_read()."
        )
      },
      paste(problems, collapse = "; ")
    ), call. = FALSE)
  }
  invisible(reference)
}

#' Read a Frozen Norm Reference From a File
#'
#' Reads a frozen reference saved by the Scoring and Norms page's Download
#' frozen reference (`.rds`), or by [saveRDS()] of the value of
#' [sieve_norm_freeze()], and refuses a file that holds anything but a
#' plain, well-formed reference. The page's Import a frozen SIEVER
#' reference (`.rds`) reads its file with this function.
#'
#' @details
#' A reference moves between sites (calibrate at one, score cohorts at
#' another), so the file is treated as coming from someone else. What it
#' held is checked type by type before any field of it is read, through
#' primitives that neither dispatch methods nor force promises (`typeof()`
#' of each element and attribute, `attributes()`, `isS4()`). Every value,
#' at any depth and in any attribute, must be `NULL`, a logical, integer,
#' double, complex, character or raw vector, or a list, and must not be an
#' S4 object; every class must be a class of data (data frames, factors,
#' dates and times, haven's labelled columns) or a result class of SIEVER,
#' DMAR or lavaan. A file that holds an environment (and so any active
#' binding or unevaluated promise in it), a function, a promise, R code, an
#' external pointer, an S4 object or another class is refused with the
#' place and the type or class found, and nothing in the file is run.
#'
#' Then the structure is checked, and a refusal names the field: the class
#' `siever_norm_reference` and its `format`; a nonempty `name` and
#' `population`; a `table` with a row per construct and the columns
#' `construct`, `mean` and `sd`; a `definition` for each construct with its
#' item names and a weight per item; each construct's calibration `scores`,
#' finite and nonempty; a `design` whose `unit` is one string; and, when
#' present, `bootstrap_scores` holding score, row and cluster of one length
#' for each construct. Whether the reference fits the data at hand (the same
#' items, versions, reversals, weights and missing-response rules) is
#' checked when it is applied ([sieve_norm_apply()]).
#'
#' A reference holds the calibration sample's scores and row numbers, and a
#' clustered one each respondent's score with that respondent's row and
#' cluster label (see [sieve_norm_freeze()]): it is respondent-level data.
#'
#' @param path The path of the `.rds` file.
#' @return The frozen reference, a list of class `siever_norm_reference`
#'   as [sieve_norm_freeze()] returns it.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' reference <- sieve_norm_freeze(items, meta, name = "Holzinger and Swineford calibration",
#'   population = "Grade 7 and 8 students in two Chicago schools, 1939")
#' path <- tempfile(fileext = ".rds")
#' saveRDS(reference, path)
#' identical(sieve_norm_read(path), reference)
#' # A file that holds anything but plain data is refused before it is read.
#' saveRDS(list(format = "SIEVER frozen norm reference 1", hook = function() "run me"), path)
#' try(sieve_norm_read(path))
#' @export
sieve_norm_read <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path) || dir.exists(path)) {
    stop("The frozen reference file was not found. Give the path of an .rds file that holds a frozen SIEVER norm reference.",
      call. = FALSE
    )
  }
  # Held in a list and checked for plain data before any field of it is read.
  read <- sieve_capture(list(readRDS(path)))
  if (!read$ok) {
    stop(sprintf(
      paste(
        "The file could not be read as an R data file (%s), so nothing was imported. It is damaged or",
        "it is another kind of file; import a reference that SIEVER's Download frozen reference (.rds) wrote."
      ),
      read$error
    ), call. = FALSE)
  }
  sieve_norm_reference_check(read$value, file = TRUE)
}

#' Apply Stored Calibration Constants Without Restandardizing a New Cohort
#'
#' @description
#' Scores new respondents with a frozen reference's scoring definition and
#' turns each score into a T score and a percentile rank with the
#' reference's calibration constants, without standardizing on the new
#' respondents. Data whose items, reversals, weights, scoring methods or
#' missing-response rules differ from the frozen definition are refused, and
#' a score outside the calibration range is labeled as such.
#'
#' @param reference A frozen reference from [sieve_norm_freeze()] or
#'   [sieve_norm_read()]; it is checked for plain data and a usable structure
#'   before any field of it is read, as [sieve_norm_read()] checks a file.
#' @param data Processed new item responses.
#' @param meta Item ledger for the new responses.
#' @param policies Current per-construct scoring policies.
#' @return A data frame with a row per case and construct: `row`,
#'   `construct`, `score`, `answered` (the items answered), `eligible`
#'   (whether a score was formed under the frozen missing-response rule),
#'   `reason` (`NA` for a scored case, otherwise why it was not scored, as
#'   [sieve_composite_score()] gives it), the fixed-reference `T`, the
#'   empirical `percentile` with its `percentile_definition`,
#'   `calibration_range`, and `reference`.
#' @author Ken Kelley
#' @examples
#' hs <- lavaan::HolzingerSwineford1939
#' items <- hs[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' calibration <- hs$school == "Pasteur"
#' reference <- sieve_norm_freeze(items[calibration, ], meta, name = "Pasteur calibration",
#'   population = "Pasteur School students, 1939")
#' # The other school, on the Pasteur reference, without restandardizing.
#' applied <- sieve_norm_apply(reference, items[!calibration, ], meta)
#' head(applied)
#' @export
sieve_norm_apply <- function(reference, data, meta, policies = list()) {
  # Plain data and a usable structure, checked before any field is read
  # (a reference read with readRDS() can hold code).
  sieve_norm_reference_check(list(reference))
  if (!identical(
    sieve_norm_definition_read(reference$definition),
    sieve_norm_definition_read(sieve_norm_definition(
      meta,
      policies
    ))
  )) stop("Items, versions, reversals, weights, score methods, or missing-response policies differ from the frozen reference. Create an explicit new reference.",
    call. = FALSE
  )
  out <- lapply(seq_len(nrow(reference$table)), function(i) {
    row <- reference$table[i, ]
    cn <- row$construct
    d <- reference$definition[[cn]]
    scored <- sieve_composite_score(data, d$items, d$weights, d$method %||% "mean", d$min_count, d$min_prop)
    score <- scored$scores
    T <- if (is.finite(row$sd) && row$sd > 0) 50 + 10 * (score - row$mean) / row$sd else rep(NA_real_, length(score))
    data.frame(
      row = seq_along(score), construct = cn, score = score,
      answered = as.integer(scored$answered), eligible = scored$eligible, reason = scored$reason,
      T = T,
      percentile = vapply(score, function(v) {
        if (!is.finite(v)) return(NA_real_)
        x <- reference$scores[[cn]]
        100 * (sum(x < v) + .5 * sum(x == v)) / length(x)
      }, numeric(1)),
      percentile_definition = "midrank: percent below plus half the percent equal in the frozen calibration sample",
      calibration_range = ifelse(!is.finite(score), NA_character_,
        ifelse(score < min(reference$scores[[cn]]), "below calibration range",
          ifelse(score > max(reference$scores[[cn]]), "above calibration range", "within calibration range")
        )
      ),
      reference = reference$name, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' The fewest resamples the calibration transformation bootstrap runs
#'
#' At least 99, and at least \eqn{2 / (1 - C) - 1} for a percentile
#' interval at level \eqn{C} (`sieve_bootstrap_min_draws()`, 199 at 99
#' percent), the floor SIEVER sets for every percentile interval: with
#' fewer, even the most extreme resample cuts off on average more than the
#' tail a limit stands for (see `sieve_bootstrap_min_draws()`). The type 7
#' limits used here lie within the resamples at any count, so the floor is
#' not a condition for computing them.
#'
#' @param level A confidence level strictly between 0 and 1.
#' @return One integer.
#' @author Ken Kelley
#' @noRd
sieve_norm_min_resamples <- function(level) {
  max(99L, sieve_bootstrap_min_draws(level))
}

#' Calibration Uncertainty for a Frozen Linear T Transformation
#'
#' @description
#' A frozen norm reference turns a raw construct score into a T score with
#' the calibration sample's mean and standard deviation. Those two constants
#' are estimates, so the T score a given raw score receives would differ
#' with another calibration sample. This function gives percentile
#' bootstrap intervals for the mean and the standard deviation of each
#' construct's calibration scores and for the T scores of three fixed raw
#' scores, so that the calibration's sampling uncertainty can be reported
#' beside the norms.
#'
#' @details
#' **Targets.** For each construct: the calibration `mean` and `sd`, and
#' `T(x) = 50 + 10 * (x - m) / s` at three fixed raw scores `x`, the
#' calibration sample's 5th, 50th and 95th percentiles (type 7 quantiles,
#' held fixed across resamples; `fixed_score`), with `m` and `s` recomputed
#' in every resample.
#'
#' **Point values.** `estimate` is each target's value in the calibration
#' sample itself: the `mean` and `sd` of its scores, and for a fixed score
#' `50 + 10 * (fixed_score - mean) / sd` from them, so each interval stands
#' beside the value it is an interval for.
#'
#' **Method.** A percentile bootstrap (Efron & Tibshirani, 1993) of the
#' calibration rows, or of whole clusters when the reference was frozen
#' with cluster labels, so that the dependence within a cluster is kept;
#' `method`, `clusters` and `caution` say which unit was resampled and warn
#' when few clusters were. Each interval is pointwise (it is not a
#' simultaneous band over the three scores), and it describes the
#' calibration's sampling variability, not the measurement error in one
#' respondent's score. A limit is the type 7 quantile of the finite
#' resampled values (a resample whose SD is zero gives no T score). An
#' interval is withheld (`NA` limits) when fewer than 90 percent of the
#' resamples gave a finite value, or fewer than `2 / (1 - conf_level) - 1`
#' did (39 at 0.95; `successful` and `failed` count them). That count is
#' the floor SIEVER sets for every percentile interval. Its basis is the
#' order statistics: the smallest of `B` resamples cuts off, on average, a
#' share `1 / (B + 1)` of the bootstrap distribution, so with fewer than
#' `2 / (1 - conf_level) - 1` resamples every limit read from them cuts
#' off, on average, more than the tail of `(1 - conf_level) / 2` it stands
#' for. It is also the fewest at which boot's percentile rule, which places
#' a limit at position `(B + 1)(1 - conf_level) / 2` among the ordered
#' resamples, reaches the first of them. The type 7 limits used here lie
#' within the resamples at any count, so the floor is not a condition for
#' computing them; it is necessary, not sufficient, for a limit to reach
#' its tail. The intervals are approximate, with no coverage guarantee. The
#' number of resamples sets the Monte Carlo precision of the limits, not
#' what they estimate.
#'
#' **Seed.** With `seed = NULL` one integer is drawn from the session's
#' random stream and used; it is returned in `seed` (with `seed_supplied =
#' FALSE`), so the run can be repeated exactly. The resampling runs in a
#' local stream (`withr::with_seed()`), so beyond that one draw the
#' session's random stream is left as it was.
#'
#' **Refusals** (an error): a reference that fails the plain-data check; a
#' clustered reference frozen without its cluster labels, or a reference of
#' repeated observations (use a longitudinal reference); a `conf_level`
#' outside (0, 1); and `B` that is not a whole number of at least 99 and at
#' least `2 / (1 - conf_level) - 1` (199 at 0.99).
#'
#' @param reference A frozen reference from [sieve_norm_freeze()] or
#'   [sieve_norm_read()]; it is checked for plain data and a usable structure
#'   before any field of it is read, as [sieve_norm_read()] checks a file.
#' @param B Number of bootstrap resamples, one whole number of at least 99
#'   and at least `2 / (1 - conf_level) - 1`.
#' @param seed Isolated reproducible seed, or `NULL` to draw one (see
#'   Details).
#' @param conf_level Percentile bootstrap confidence level: one number
#'   strictly between 0 and 1 (0.95 for a 95 percent interval).
#' @return A data frame with five rows per construct, one per target:
#'   \describe{
#'     \item{`construct`}{The construct.}
#'     \item{`target`}{`"mean"`, `"sd"`, `"T at calibration P5"`,
#'       `"T at calibration P50"` or `"T at calibration P95"`.}
#'     \item{`fixed_score`}{The raw score held fixed, for a T target;
#'       `NA` for the mean and the SD.}
#'     \item{`estimate`}{The target's value in the calibration sample.}
#'     \item{`lower`, `upper`}{The percentile limits, or `NA` when the
#'       interval is withheld.}
#'     \item{`conf_level`}{The confidence level.}
#'     \item{`requested`, `successful`, `failed`}{Resamples requested, and
#'       those that gave a finite value or did not.}
#'     \item{`seed`, `seed_supplied`}{The seed used, and whether it was
#'       supplied.}
#'     \item{`clusters`}{The number of clusters resampled, or `NA` for
#'       independent rows.}
#'     \item{`caution`}{A caution about the resampling unit, or `""`.}
#'     \item{`method`}{The method, in words.}
#'   }
#'   All values are at full precision.
#' @references
#' Efron, B., & Tibshirani, R. J. (1993). *An introduction to the
#' bootstrap*. Chapman and Hall. \doi{10.1007/978-1-4899-4541-9}
#' @seealso [sieve_norm_freeze()], [sieve_norm_apply()]
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' reference <- sieve_norm_freeze(items, meta, name = "Holzinger and Swineford calibration",
#'   population = "Grade 7 and 8 students in two Chicago schools, 1939")
#' limits <- sieve_norm_transform_limits(reference, B = 199L, seed = 1)
#' limits[, c("construct", "target", "fixed_score", "estimate", "lower", "upper", "successful")]
#' @export
sieve_norm_transform_limits <- function(reference, B = 999L, seed = NULL, conf_level = .95) {
  conf_level <- sieve_check_conf_level(conf_level)
  sieve_norm_reference_check(list(reference))
  unit <- reference$design$unit %||% "independent"
  if (!unit %in% c("independent", "clustered") ||
      (identical(unit, "clustered") && is.null(reference$bootstrap_scores))) {
    stop("This reference has no aligned cluster labels for whole-cluster calibration intervals. Freeze it again with cluster labels; repeated observations need a longitudinal reference.", call. = FALSE)
  }
  required <- sieve_norm_min_resamples(conf_level)
  if (!is.numeric(B) || length(B) != 1L || !is.finite(B) || B != trunc(B) || B < required) {
    stop(sprintf(
      paste(
        "`B` must be one whole number of at least %d: at least 99 resamples, and for a percentile interval",
        "at %s%% at least 2 / (1 - %s) - 1 = %d, the floor SIEVER sets for every percentile interval",
        "(with fewer, even the most extreme resample cuts off on average more than the tail of %s a limit",
        "stands for); %s was given."
      ),
      required, format(100 * conf_level), format(conf_level), sieve_bootstrap_min_draws(conf_level),
      format((1 - conf_level) / 2), sieve_given_text(B)
    ), call. = FALSE)
  }
  minimum_used <- sieve_bootstrap_min_draws(conf_level)
  rng <- sieve_draw_seed(seed)
  withr::with_seed(rng$seed, do.call(rbind, lapply(names(reference$scores), function(cn) {
    held <- reference$bootstrap_scores[[cn]]
    x <- if (identical(unit, "clustered")) held$score else reference$scores[[cn]]
    partition <- sieve_bootstrap_partition(length(x), if (identical(unit, "clustered")) held$cluster,
      if (identical(unit, "clustered")) held$rows else seq_along(x))
    points <- as.numeric(stats::quantile(x, c(.05, .5, .95), type = 7))
    # The point estimates, from the calibration sample itself.
    m0 <- mean(x)
    s0 <- stats::sd(x)
    estimates <- c(m0, s0, if (is.finite(s0) && s0 > 0) 50 + 10 * (points - m0) / s0 else rep(NA_real_, 3))
    draws <- replicate(B, {
      z <- x[sieve_bootstrap_rows(partition)]
      m <- mean(z)
      s <- stats::sd(z)
      c(mean = m, sd = s, if (s > 0) 50 + 10 * (points - m) / s else rep(NA_real_, 3))
    })
    targets <- c("mean", "sd", "T at calibration P5", "T at calibration P50", "T at calibration P95")
    do.call(rbind, lapply(seq_along(targets), function(j) {
      v <- draws[j, ]
      ok <- is.finite(v)
      # An interval is formed from at least 90 percent of the resamples and
      # at least the fewest a percentile limit needs.
      interval <- if (sum(ok) >= .9 * B && sum(ok) >= minimum_used) stats::quantile(v[ok],
        c(
          (1 - conf_level) / 2,
          (1 + conf_level) / 2
        ),
        names = FALSE
      ) else c(
        NA_real_,
        NA_real_
      )
      data.frame(
        construct = cn,
        target = targets[j],
        fixed_score = c(
          NA,
          NA,
          points
        )[j],
        estimate = estimates[j],
        lower = interval[1],
        upper = interval[2],
        conf_level = conf_level,
        requested = B, successful = sum(ok), failed = sum(!ok), seed = rng$seed, seed_supplied = rng$supplied,
        clusters = partition$G,
        caution = partition$caution %||% "",
        method = sprintf(
          paste(
            "Percentile bootstrap of calibration constants using %s; an interval is formed when at least",
            "90 percent of the resamples, and at least %d, give a finite value; approximate, no coverage guarantee"
          ),
          sieve_bootstrap_unit_text(partition), minimum_used
        ),
        stringsAsFactors = FALSE
      )
    }))
  })))
}

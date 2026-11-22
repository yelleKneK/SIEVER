# fct_expectations.R
# Validity evidence read against the researcher's own predictions. A
# correlation between a construct score and an external variable is
# evidence about validity only in relation to what theory said it should be
# (Cronbach and Meehl, 1955; Campbell and Fiske, 1959). Three kinds of
# expectation are therefore kept apart and never inferred from one another:
#
# * unspecified: the researcher made no prediction. The estimate is
#   reported and is not scored in any way. The absence of a prediction is
#   not a prediction of "small".
# * directional: a predicted sign, optionally with a predicted minimum
#   magnitude. A correlation of the wrong sign is never read as agreeing
#   with it, whatever its size.
# * discriminant: predicted to be small, within a bound the researcher
#   states.
#
# Each estimate is described by where its confidence interval lies in
# relation to the stated proposition. Nothing here prints a verdict: a
# fixed cutoff on the absolute correlation cannot establish validity, and
# an interval that agrees with a prediction is evidence, not proof.
#
# The stated predictions are the researcher's propositions and are kept as
# data, one row per construct and criterion, with the kind of prediction,
# the smallest magnitude of a direction, and the bound of a discriminant
# prediction stored separately (the sieve_*_prediction functions below).
# The page draws its controls from that store, so a prediction is not lost
# when the controls are rebuilt.

#' A score or a criterion, read as numbers
#'
#' The one reading of the values a validity coefficient is computed from,
#' the reading the Validity page's criterion gate makes
#' (`sieve_numeric_columns()`): a factor is read by its labels, never by
#' its level codes (`factor(c("5", "10", "20"))` holds 5, 10 and 20, whose
#' codes in sorted level order are 3, 1 and 2, so a correlation with the
#' codes can have the wrong sign); numerals stored as text are read as
#' those numbers; a blank entry is a missing value; and a number column,
#' including one read from SPSS or Stata, gives its values through
#' `sieve_as_number()`, with any declared missing values set to missing.
#' An entry that is present and is not a number is not set to missing
#' quietly, which would correlate the cases that remain: the vector is
#' refused, with the count and an example.
#'
#' @param v The values, one per case.
#' @param what How the message names `v` (`` "`y`" ``, or "Criterion
#'   'age'").
#' @return A double vector as long as `v`; an error when `v` holds an entry
#'   that is not a number or is not a vector of values.
#' @author Ken Kelley
#' @noRd
sieve_criterion_values <- function(v, what = "`y`") {
  if (!is.null(v) && (!is.atomic(v) || length(dim(v)) > 1L && NCOL(v) != 1L)) {
    stop(sprintf(
      "%s must be one value per case (a vector, or one column of a data frame), not an object of class %s.",
      what, paste(class(v), collapse = "/")
    ), call. = FALSE)
  }
  if (is.factor(v)) {
    v <- as.character(v)
  }
  if (!is.character(v)) {
    return(sieve_as_number(v))
  }
  v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
  num <- suppressWarnings(as.numeric(v))
  lost <- which(!is.na(v) & is.na(num))
  if (length(lost)) {
    stop(sprintf(
      paste0(
        "%s holds %d %s (for example \"%s\"); score or recode it as numbers before correlating. ",
        "A factor is read by its labels, never by its level codes."
      ),
      what, length(lost),
      if (length(lost) == 1L) "value that is not a number" else "values that are not numbers",
      v[lost[1L]]
    ), call. = FALSE)
  }
  num
}

#' Correlation Between a Score and a Criterion, With Its Interval
#'
#' The Pearson correlation of the complete pairs and its Fisher z
#' confidence interval, from [stats::cor.test()]. Nothing is rounded. When
#' the estimate or the interval cannot be computed, the reason is kept.
#'
#' @details
#' Uses finite paired responses after numeric conversion. A factor is read
#' by its labels, never by its level codes, so `factor(c("5", "10", "20"))`
#' holds 5, 10 and 20 (the codes, in sorted level order, would be 3, 1 and
#' 2, and a correlation with them can have the wrong sign); numerals stored
#' as text are read as those numbers and a blank entry is a missing value.
#' An entry that is present and is not a number (`"n/a"`, `"7 (approx.)"`,
#' a label such as `"Agree"`) stops the function with the count and an
#' example, rather than becoming a missing value that leaves the remaining
#' cases to be correlated; score or recode such a variable first. `x` and
#' `y` must be the same cases in the same order, so vectors of different
#' lengths are refused. The Pearson
#' correlation and Fisher z interval assume independent pairs and the usual
#' bivariate-distribution approximation. A computed coefficient does not
#' establish the intended interpretation of either variable. No dependent-design
#' correction is supplied: when the declared sampling unit is not independent
#' rows (clustered or repeated observations; `design`), the correlation is
#' reported descriptively, with no interval, no p-value, and the reason, so no
#' prediction is scored against an interval the design does not support.
#' Insufficient pairs or an undefined coefficient retain a reason rather than
#' an invented interval.
#' @param x,y Vectors of equal length, one value per case, already scored
#'   as intended: numbers, numerals stored as text, or a factor whose labels
#'   are numbers (read by its labels). Text entries that are not numbers are
#'   refused.
#' @param conf_level Confidence level of the interval, a single number
#'   strictly between 0 and 1.
#' @param design `NULL` for independent rows, or the sentence that says why
#'   the declared sampling design does not support the interval (from
#'   `sieve_sampling_problem()`).
#' @return A list with `r`, `lower`, `upper`, `n` (complete pairs), `p`,
#'   `conf_level`, `method`, and `reason` (`NA` when the estimate and the
#'   interval are both available).
#' @author Ken Kelley
#' @examples
#' hs <- lavaan::HolzingerSwineford1939
#' visual <- rowMeans(hs[, c("x1", "x2", "x3")])
#' sieve_validity_estimate(visual, hs$ageyr)
#' @export
sieve_validity_estimate <- function(x, y, conf_level = 0.95, design = NULL) {
  conf_level <- sieve_check_conf_level(conf_level)
  x <- sieve_criterion_values(x, "`x`")
  y <- sieve_criterion_values(y, "`y`")
  if (length(x) != length(y)) {
    stop(sprintf(
      "`x` has %d values and `y` has %d; they must be the same cases in the same order.",
      length(x), length(y)
    ), call. = FALSE)
  }
  ok <- is.finite(x) & is.finite(y)
  out <- list(
    r = NA_real_, lower = NA_real_, upper = NA_real_, n = sum(ok),
    p = NA_real_, conf_level = conf_level,
    method = "Pearson correlation; Fisher z interval",
    reason = NA_character_
  )
  if (out$n < 3L) {
    out$reason <- sprintf(
      "Only %d complete pair%s; a correlation needs at least 3.",
      out$n, if (out$n == 1L) "" else "s"
    )
    return(out)
  }
  if (!is.null(design)) {
    # Descriptive only: the Fisher z interval and the p-value assume
    # independent rows, which the declared design says these are not.
    run <- sieve_capture(stats::cor(x[ok], y[ok]))
    out$method <- "Pearson correlation; descriptive, no interval under the declared sampling design"
    out$r <- if (run$ok && is.finite(run$value)) run$value else NA_real_
    out$reason <- paste(
      if (is.finite(out$r)) "Descriptive correlation only:" else "The correlation is undefined:",
      design
    )
    return(out)
  }
  run <- sieve_capture(stats::cor.test(x[ok], y[ok], conf.level = conf_level))
  if (!run$ok) {
    out$reason <- run$error
    return(out)
  }
  ct <- run$value
  out$r <- unname(ct$estimate)
  out$p <- ct$p.value
  if (!is.finite(out$r)) {
    out$reason <- if (length(run$warnings)) {
      paste(run$warnings, collapse = " ")
    } else {
      "The correlation is undefined (a variable without variance)."
    }
    return(out)
  }
  if (is.null(ct$conf.int) || !all(is.finite(ct$conf.int))) {
    out$reason <- sprintf(
      "Interval unavailable: the Fisher z interval needs at least 4 complete pairs (%d here).",
      out$n
    )
    return(out)
  }
  out$lower <- ct$conf.int[1]
  out$upper <- ct$conf.int[2]
  out
}

#' Where an interval lies in relation to a stated prediction
#'
#' The classification behind the criterion table, as a plain function of
#' one estimate, its interval, and one expectation.
#'
#' Directional prediction (sign `+1` or `-1`): the interval is entirely on
#' the predicted side of zero, includes zero, or is entirely on the
#' opposite side. With a predicted minimum magnitude `min_abs`, an interval
#' on the predicted side is further described as entirely at or beyond the
#' minimum, including it, or entirely short of it.
#'
#' Discriminant prediction (the correlation is smaller in absolute value
#' than `bound`): the interval lies entirely inside `(-bound, bound)`,
#' includes a bound, or lies entirely outside.
#'
#' No prediction: not scored. A missing estimate or a missing interval is
#' reported as unavailable, and a prediction that is not fully stated (no
#' sign, or no usable bound) is reported as incomplete; none of these is
#' scored.
#'
#' @param estimate,lower,upper The estimate and its interval limits.
#' @param kind `"unspecified"`, `"directional"`, or `"discriminant"`.
#' @param sign `1` or `-1` for a directional prediction.
#' @param min_abs Optional predicted minimum magnitude, in `(0, 1]`.
#' @param bound The researcher's bound for a discriminant prediction, in
#'   `(0, 1]`.
#' @return A list with `kind`, `position` (a short code), `scored`
#'   (whether a stated prediction could be related to an interval), and
#'   `statement` (a sentence describing the position, without a verdict).
#' @author Ken Kelley
#' @noRd
sieve_validity_classify <- function(estimate, lower, upper,
                                    kind = c("unspecified", "directional", "discriminant"),
                                    sign = NA, min_abs = NA, bound = NA) {
  kind <- match.arg(kind)
  out <- function(position, statement, scored = FALSE) {
    list(kind = kind, position = position, scored = scored, statement = statement)
  }
  num <- sieve_magnitude_text
  if (identical(kind, "unspecified")) {
    return(out("not_scored", "No prediction stated: reported descriptively and not scored."))
  }
  side <- NULL
  if (identical(kind, "directional")) {
    if (length(sign) != 1L || is.na(sign) || !sign %in% c(-1, 1)) {
      return(out(
        "prediction_incomplete",
        "The predicted direction is not stated, so the estimate is not scored."
      ))
    }
    if (!is.na(min_abs) && !(is.finite(min_abs) && min_abs > 0 && min_abs <= 1)) {
      return(out(
        "prediction_incomplete",
        "The predicted minimum magnitude must lie between 0 and 1, so the estimate is not scored."
      ))
    }
    side <- if (sign > 0) "positive" else "negative"
  } else if (length(bound) != 1L || !is.finite(bound) || bound <= 0 || bound > 1) {
    return(out(
      "prediction_incomplete",
      "A bound between 0 and 1 is needed to read the interval against it, so the estimate is not scored."
    ))
  }
  if (length(estimate) != 1L || !is.finite(estimate)) {
    return(out("unavailable", "Estimate unavailable, so there is nothing to read against the prediction."))
  }
  if (!is.finite(lower) || !is.finite(upper)) {
    return(out(
      "interval_unavailable",
      "Interval unavailable: the estimate cannot be related to the prediction without its uncertainty."
    ))
  }
  if (identical(kind, "directional")) {
    # Reflect a negative prediction so that the predicted side is positive.
    lo <- if (sign > 0) lower else -upper
    hi <- if (sign > 0) upper else -lower
    lead <- if (is.na(min_abs)) {
      sprintf("Predicted %s.", side)
    } else {
      sprintf("Predicted %s, at least %s in magnitude.", side, num(min_abs))
    }
    if (hi < 0) {
      return(out("opposite_side", paste(
        lead, sprintf(
          "The interval lies entirely on the %s side of zero, opposite to the prediction.",
          if (sign > 0) "negative" else "positive"
        )
      ), TRUE))
    }
    if (lo <= 0) {
      return(out("straddles_zero", paste(
        lead, "The interval includes zero, so these data do not settle the direction."
      ), TRUE))
    }
    if (is.na(min_abs)) {
      return(out("predicted_side", paste(
        lead, "The interval lies entirely on the predicted side of zero."
      ), TRUE))
    }
    if (lo >= min_abs) {
      return(out("at_or_beyond_minimum", paste(
        lead, "The interval lies entirely on the predicted side and at or beyond the predicted minimum."
      ), TRUE))
    }
    if (hi < min_abs) {
      return(out("below_minimum", paste(
        lead, "The interval lies entirely on the predicted side of zero but short of the predicted minimum."
      ), TRUE))
    }
    return(out("straddles_minimum", paste(
      lead, "The interval lies on the predicted side of zero and includes the predicted minimum."
    ), TRUE))
  }
  lead <- sprintf("Predicted small: within plus or minus %s.", num(bound))
  if (lower > -bound && upper < bound) {
    return(out("inside_bound", paste(lead, "The interval lies entirely inside your bound."), TRUE))
  }
  if (lower >= bound || upper <= -bound) {
    return(out("outside_bound", paste(lead, "The interval lies entirely outside your bound."), TRUE))
  }
  out("straddles_bound", paste(
    lead, "The interval includes your bound, so these data do not settle whether the relation is that small."
  ), TRUE)
}

#' Read a Table of Estimates Against a Table of Expectations
#'
#' @description
#' Sets each estimated construct-criterion correlation, with its interval,
#' beside the prediction the researcher stated for that pair (its kind, sign
#' and minimum size), and says where the interval lies in relation to the
#' prediction. A pair without a prediction is reported and not scored; the
#' reading states a position, not a verdict on validity.
#'
#' @details
#' Directional predictions use `sign` equal to -1 or 1 and optional `min_abs`;
#' discriminant predictions use a positive `bound` on the absolute correlation.
#' A supplied `min_abs` or `bound` must be greater than zero and at most one.
#' `kind` is `"unspecified"`, `"directional"`, or `"discriminant"`. Predictions
#' must be supplied by the researcher, not inferred from observed estimates.
#' Missing or incomplete predictions are explicitly unscored. This reads
#' existing intervals; it neither fits a model nor changes their method or
#' confidence level. Statements describe interval positions, not validity
#' verdicts. Inputs and returned estimates retain their precision.
#' @param estimates A data frame with `construct`, `criterion`, `r`,
#'   `lower`, `upper`, and optionally `n`, `p`, and `reason`.
#' @param expectations A data frame with `construct`, `criterion`, `kind`,
#'   `sign`, `min_abs`, and `bound`. A pair that is absent has no
#'   prediction: it is unspecified, never discriminant.
#' @return `estimates` with `kind`, `sign`, `min_abs`, `bound`, `position`,
#'   `scored`, and `statement` added, at full precision.
#' @author Ken Kelley
#' @examples
#' hs <- lavaan::HolzingerSwineford1939
#' textual <- rowMeans(hs[, c("x4", "x5", "x6")])
#' speed <- rowMeans(hs[, c("x7", "x8", "x9")])
#' estimates <- do.call(rbind, lapply(list(Textual = textual, Speed = speed), function(score) {
#'   as.data.frame(sieve_validity_estimate(score, hs$grade)[c("r", "lower", "upper", "n")])
#' }))
#' estimates <- cbind(construct = c("Textual", "Speed"), criterion = "grade", estimates)
#' expectations <- data.frame(construct = "Textual", criterion = "grade",
#'   kind = "directional", sign = 1, min_abs = .1, bound = NA)
#' sieve_validity_evidence(estimates, expectations)[, c("construct", "kind", "position", "statement")]
#' @export
sieve_validity_evidence <- function(estimates, expectations = NULL) {
  out <- estimates
  n <- nrow(out)
  out$kind <- rep("unspecified", n)
  out$sign <- rep(NA_real_, n)
  out$min_abs <- rep(NA_real_, n)
  out$bound <- rep(NA_real_, n)
  if (!is.null(expectations) && nrow(expectations) && n) {
    at <- match(
      paste(out$construct, out$criterion, sep = "\r"),
      paste(expectations$construct, expectations$criterion, sep = "\r")
    )
    hit <- !is.na(at)
    out$kind[hit] <- as.character(expectations$kind[at[hit]])
    out$sign[hit] <- as.numeric(expectations$sign[at[hit]])
    out$min_abs[hit] <- as.numeric(expectations$min_abs[at[hit]])
    out$bound[hit] <- as.numeric(expectations$bound[at[hit]])
  }
  read <- lapply(seq_len(n), function(i) {
    cls <- sieve_validity_classify(
      out$r[i], out$lower[i], out$upper[i], out$kind[i],
      out$sign[i], out$min_abs[i], out$bound[i]
    )
    why <- if ("reason" %in% names(out)) out$reason[i] else NA_character_
    if (cls$position %in% c("unavailable", "interval_unavailable") && !is.na(why)) {
      cls$statement <- paste(cls$statement, why)
    }
    cls
  })
  out$position <- vapply(read, function(x) x$position, character(1))
  out$scored <- vapply(read, function(x) x$scored, logical(1))
  out$statement <- vapply(read, function(x) x$statement, character(1))
  out
}

#' Count the positions of the stated predictions
#'
#' A description of the table, kept apart by kind of prediction. Pairs
#' without a prediction are counted as such and enter no other count, and a
#' prediction whose estimate or interval is unavailable is counted as
#' unavailable, never as agreeing or disagreeing.
#'
#' @param evidence The data frame from `sieve_validity_evidence()`.
#' @return A list with `directional` and `discriminant` (named integer
#'   vectors of position counts over the scored rows of that kind),
#'   `unspecified` (the number of pairs without a prediction), and
#'   `unavailable` (stated predictions that could not be scored).
#' @author Ken Kelley
#' @noRd
sieve_validity_tally <- function(evidence) {
  count <- function(kind, levels) {
    rows <- evidence$position[evidence$kind == kind & evidence$scored]
    stats::setNames(vapply(levels, function(l) sum(rows == l), integer(1)), levels)
  }
  list(
    directional = count("directional", c(
      "predicted_side", "at_or_beyond_minimum", "straddles_minimum",
      "below_minimum", "straddles_zero", "opposite_side"
    )),
    discriminant = count("discriminant", c("inside_bound", "straddles_bound", "outside_bound")),
    unspecified = sum(evidence$kind == "unspecified"),
    unavailable = sum(evidence$kind != "unspecified" & !evidence$scored)
  )
}

#' The tally of stated predictions as sentences
#'
#' With the evidence table given, each count is followed by the pairs it
#' counts, named by construct and criterion with the prediction stated for
#' them, so the tally reads as sentences about the researcher's own
#' predictions and not as bare numbers.
#'
#' @param tally The list from `sieve_validity_tally()`.
#' @param conf_level Confidence level of the intervals.
#' @param evidence The data frame from `sieve_validity_evidence()` the tally
#'   was counted from, or `NULL` for the counts alone.
#' @return A character vector of sentences (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_validity_tally_text <- function(tally, conf_level = 0.95, evidence = NULL) {
  pct <- sprintf("%s%%", format(100 * conf_level))
  d <- tally$directional
  on_side <- d[["predicted_side"]] + d[["at_or_beyond_minimum"]] + d[["straddles_minimum"]] +
    d[["below_minimum"]]
  n_dir <- sum(d)
  n_dis <- sum(tally$discriminant)
  with_min <- d[["at_or_beyond_minimum"]] + d[["straddles_minimum"]] + d[["below_minimum"]]
  # The pairs behind a count, each named with its prediction.
  named <- function(rows) {
    if (is.null(rows) || !nrow(rows)) {
      return(character(0))
    }
    sprintf(
      "%s with %s (%s)", rows$construct, rows$criterion,
      unname(mapply(
        sieve_validity_prediction_text, rows$kind, rows$sign,
        rows$min_abs, rows$bound
      ))
    )
  }
  at <- function(kind, positions, scored = TRUE) {
    if (is.null(evidence)) {
      return(NULL)
    }
    rows <- evidence[evidence$kind %in% kind & evidence$scored == scored &
      evidence$position %in% positions, , drop = FALSE]
    named(rows)
  }
  listed <- function(label, who) {
    if (is.null(who) || !length(who)) "" else sprintf(" %s: %s.", label, paste(who, collapse = "; "))
  }
  c(
    if (n_dir) {
      paste0(
        sprintf(
          paste0(
            "Directional predictions (%d): the %s interval lies entirely on the predicted side ",
            "of zero for %d, includes zero for %d, and lies entirely on the opposite side for %d."
          ),
          n_dir, pct, on_side, d[["straddles_zero"]], d[["opposite_side"]]
        ),
        if (with_min) {
          sprintf(
            paste0(
              " Of the %d on the predicted side that carry a predicted minimum, the interval is ",
              "entirely at or beyond it for %d, includes it for %d, and is entirely short of it for %d."
            ),
            with_min, d[["at_or_beyond_minimum"]], d[["straddles_minimum"]], d[["below_minimum"]]
          )
        } else {
          ""
        },
        listed("On the predicted side", at("directional", "predicted_side")),
        listed(
          "On the predicted side and at or beyond the predicted minimum",
          at("directional", "at_or_beyond_minimum")
        ),
        listed(
          "On the predicted side and including the predicted minimum",
          at("directional", "straddles_minimum")
        ),
        listed(
          "On the predicted side but short of the predicted minimum",
          at("directional", "below_minimum")
        ),
        listed("Including zero", at("directional", "straddles_zero")),
        listed("On the opposite side", at("directional", "opposite_side"))
      )
    },
    if (n_dis) {
      paste0(
        sprintf(
          paste0(
            "Discriminant predictions (%d): the %s interval lies entirely inside your bound for %d, ",
            "includes the bound for %d, and lies entirely outside it for %d."
          ),
          n_dis, pct, tally$discriminant[["inside_bound"]],
          tally$discriminant[["straddles_bound"]], tally$discriminant[["outside_bound"]]
        ),
        listed("Inside the bound", at("discriminant", "inside_bound")),
        listed("Including the bound", at("discriminant", "straddles_bound")),
        listed("Outside the bound", at("discriminant", "outside_bound"))
      )
    },
    if (tally$unavailable) {
      paste0(
        sprintf(
          "Stated predictions that could not be read (estimate or interval unavailable, or the prediction incomplete): %d.",
          tally$unavailable
        ),
        listed("Not read", at(c("directional", "discriminant"),
          c("unavailable", "interval_unavailable", "prediction_incomplete"),
          scored = FALSE
        ))
      )
    },
    if (tally$unspecified) {
      paste0(
        sprintf(
          "Pairs with no prediction: %d%s. They are reported and enter none of the counts above.",
          tally$unspecified,
          if (is.null(evidence)) {
            ""
          } else {
            rows <- evidence[evidence$kind == "unspecified", , drop = FALSE]
            sprintf(" (%s)", paste(sprintf("%s with %s", rows$construct, rows$criterion), collapse = ", "))
          }
        )
      )
    }
  )
}

#' A stated prediction in words
#'
#' @param kind,sign,min_abs,bound One expectation.
#' @param num The function that writes a magnitude: `sieve_magnitude_text()`
#'   for the page, `sieve_num_lit()` for an R literal.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_validity_prediction_text <- function(kind, sign = NA, min_abs = NA, bound = NA,
                                           num = sieve_magnitude_text) {
  if (identical(kind, "directional") && !is.na(sign)) {
    return(paste0(
      if (sign > 0) "positive" else "negative",
      if (!is.na(min_abs)) sprintf(", |r| at least %s", num(min_abs)) else ""
    ))
  }
  if (identical(kind, "discriminant")) {
    return(if (is.na(bound)) "small (no bound stated)" else sprintf("small, |r| below %s", num(bound)))
  }
  "none stated"
}

#' A magnitude the researcher entered, as text
#'
#' The researcher's own number is written back exactly as entered, in the
#' table, in the reading, and in the comments of the reproducible code: the
#' decimal digits that identify the double, as few as do and at most 17
#' (`sieve_num_text()`). It is a stated proposition, not an estimate, so it
#' is never rounded for display. The R literal that the recorded code holds
#' is written by `sieve_num_lit()`, which is exact by construction.
#'
#' @param x A single number.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_magnitude_text <- function(x) {
  sieve_num_text(x)
}

#' The controls of a pair for which nothing has been stated
#'
#' @return A list with `choice` (`"unspecified"`), `min_abs` (`NA`), and
#'   `bound` (0.30, the starting value of the bound control: Cohen's, 1988,
#'   conventional medium correlation, shown as a convention to be replaced).
#' @author Ken Kelley
#' @noRd
sieve_prediction_default <- function() {
  list(choice = "unspecified", min_abs = NA_real_, bound = 0.30)
}

#' The choices of the prediction control
#'
#' @return A character vector of the four values the control can hold.
#' @author Ken Kelley
#' @noRd
sieve_prediction_choices <- function() {
  c("unspecified", "positive", "negative", "discriminant")
}

#' An empty store of stated predictions
#'
#' The researcher's predictions are kept in a store, one row per construct
#' and criterion, and the controls on the page are drawn from it. They are
#' not left in the controls themselves: the controls are rebuilt whenever
#' the selected criteria or the constructs change, and a rebuilt control
#' holds only what it is given. The three parts of a prediction are stored
#' separately: the kind chosen (`choice`), the smallest magnitude that
#' goes with a direction (`min_abs`), and the bound that goes with a
#' discriminant prediction (`bound`). Changing the kind therefore does not
#' discard a magnitude entered for another kind.
#'
#' @return A data frame with no rows and columns `construct`, `criterion`,
#'   `choice`, `min_abs`, and `bound`.
#' @author Ken Kelley
#' @noRd
sieve_predictions_empty <- function() {
  data.frame(
    construct = character(0), criterion = character(0), choice = character(0),
    min_abs = numeric(0), bound = numeric(0), stringsAsFactors = FALSE
  )
}

#' Read one pair from the store of stated predictions
#'
#' @param store The store (see `sieve_predictions_empty()`), or `NULL`.
#' @param construct,criterion Names of the construct and of the criterion.
#' @return A list with `choice`, `min_abs`, and `bound`; the defaults of
#'   `sieve_prediction_default()` when the pair is not in the store.
#' @author Ken Kelley
#' @noRd
sieve_get_prediction <- function(store, construct, criterion) {
  if (!is.null(store) && nrow(store)) {
    at <- which(store$construct == construct & store$criterion == criterion)
    if (length(at)) {
      return(list(
        choice = store$choice[at[1]], min_abs = store$min_abs[at[1]],
        bound = store$bound[at[1]]
      ))
    }
  }
  sieve_prediction_default()
}

#' Write one pair to the store of stated predictions
#'
#' Only the parts that are supplied are written; a part left `NULL` keeps
#' its stored value (or takes the default for a new pair). That is how a
#' control that the page has not yet reported is told apart from a control
#' the researcher cleared: a cleared magnitude arrives as `NA` and is
#' stored as missing.
#'
#' @param store The store, or `NULL` for an empty one.
#' @param construct,criterion Names of the construct and of the criterion.
#' @param choice One of `sieve_prediction_choices()`, or `NULL`.
#' @param min_abs,bound A single number or `NA`, or `NULL`.
#' @return The store with that pair written.
#' @author Ken Kelley
#' @noRd
sieve_set_prediction <- function(store, construct, criterion,
                                 choice = NULL, min_abs = NULL, bound = NULL) {
  store <- if (is.null(store)) sieve_predictions_empty() else store
  now <- sieve_get_prediction(store, construct, criterion)
  number <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || !is.finite(x)) NA_real_ else x
  }
  if (!is.null(choice)) {
    if (length(choice) != 1L || is.na(choice) || !choice %in% sieve_prediction_choices()) {
      stop("`choice` must be one of: ", paste(sieve_prediction_choices(), collapse = ", "), ".",
        call. = FALSE
      )
    }
    now$choice <- as.character(choice)
  }
  if (!is.null(min_abs)) {
    now$min_abs <- number(min_abs)
  }
  if (!is.null(bound)) {
    now$bound <- number(bound)
  }
  at <- which(store$construct == construct & store$criterion == criterion)
  if (length(at)) {
    store$choice[at[1]] <- now$choice
    store$min_abs[at[1]] <- now$min_abs
    store$bound[at[1]] <- now$bound
    return(store)
  }
  added <- data.frame(
    construct = as.character(construct), criterion = as.character(criterion),
    choice = now$choice, min_abs = now$min_abs, bound = now$bound,
    stringsAsFactors = FALSE
  )
  out <- rbind(store, added)
  rownames(out) <- NULL
  out
}

#' Remove one pair from the store of stated predictions
#'
#' The row is removed, magnitudes included: the pair is then a pair for
#' which nothing has been stated.
#'
#' @param store The store, or `NULL`.
#' @param construct,criterion Names of the construct and of the criterion.
#' @return The store without that pair.
#' @author Ken Kelley
#' @noRd
sieve_drop_prediction <- function(store, construct, criterion) {
  store <- if (is.null(store)) sieve_predictions_empty() else store
  keep <- !(store$construct == construct & store$criterion == criterion)
  out <- store[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The stored predictions as expectations
#'
#' Turns stored controls into the expectations `sieve_validity_evidence()`
#' reads: a direction carries its sign and its smallest magnitude, a
#' discriminant prediction carries its bound, and a pair without a stated
#' prediction is unspecified. A magnitude stored for a kind that is not the
#' one chosen is not applied.
#'
#' @param store The store, or `NULL`.
#' @param constructs,criteria The pairs wanted: every construct with every
#'   criterion, criterion by criterion. With both `NULL`, every pair in the
#'   store that carries a prediction.
#' @return A data frame with `construct`, `criterion`, `kind`, `sign`,
#'   `min_abs`, and `bound`, one row per pair.
#' @author Ken Kelley
#' @noRd
sieve_predictions_expectations <- function(store, constructs = NULL, criteria = NULL) {
  if (is.null(constructs) && is.null(criteria)) {
    pairs <- if (is.null(store)) sieve_predictions_empty() else store
    pairs <- pairs[pairs$choice != "unspecified", c("construct", "criterion"), drop = FALSE]
  } else {
    pairs <- data.frame(
      construct = rep(as.character(constructs), times = length(criteria)),
      criterion = rep(as.character(criteria), each = length(constructs)),
      stringsAsFactors = FALSE
    )
  }
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    held <- sieve_get_prediction(store, pairs$construct[i], pairs$criterion[i])
    directional <- held$choice %in% c("positive", "negative")
    discriminant <- identical(held$choice, "discriminant")
    data.frame(
      construct = pairs$construct[i], criterion = pairs$criterion[i],
      kind = if (directional) "directional" else if (discriminant) "discriminant" else "unspecified",
      sign = if (identical(held$choice, "positive")) 1 else if (identical(held$choice, "negative")) -1 else NA_real_,
      min_abs = if (directional) held$min_abs else NA_real_,
      bound = if (discriminant) held$bound else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      construct = character(0), criterion = character(0), kind = character(0),
      sign = numeric(0), min_abs = numeric(0), bound = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' Stated predictions for pairs that a table does not show
#'
#' A prediction stays in the store when its criterion is deselected
#' or its construct is not in the current model. Such a prediction is not
#' lost and is not applied to anything on screen; this lists it so that the
#' page and the reproducible code can say that it exists.
#'
#' @param store The store, or `NULL`.
#' @param constructs,criteria The constructs and criteria of the pairs
#'   shown (every construct with every criterion).
#' @return The expectations of the stated pairs that are not shown (see
#'   `sieve_predictions_expectations()`); no rows when there are none.
#' @author Ken Kelley
#' @noRd
sieve_predictions_elsewhere <- function(store, constructs = character(0), criteria = character(0)) {
  stated <- sieve_predictions_expectations(store)
  shown <- stated$construct %in% constructs & stated$criterion %in% criteria
  stated[!shown, , drop = FALSE]
}

#' A stable input key for a variable or construct name
#'
#' Input identifiers built from names must not collide after unsafe
#' characters are replaced, and must not shift when the selection changes
#' (a positional identifier would hand one pair's stated prediction to
#' another). The key is the cleaned name followed by a short hash of the
#' exact name.
#'
#' @param x A character vector of names.
#' @return A character vector of keys safe for use in input identifiers.
#' @author Ken Kelley
#' @noRd
sieve_input_key <- function(x) {
  vapply(as.character(x), function(nm) {
    paste0(gsub("[^A-Za-z0-9]", "_", nm), "_", substr(sieve_hash(nm), 1L, 8L))
  }, character(1), USE.NAMES = FALSE)
}

#' The R recorded for the criterion correlations
#'
#' The stated predictions are the researcher's propositions and not
#' something the code computes, so they are written as comments and, for
#' the pairs in the table, as a data frame `predictions` whose magnitudes
#' are exact R literals (`sieve_num_lit()`): a bound read back from the
#' script is the double the researcher entered. For mean scores the
#' correlations themselves are written as runnable R from the item ledger
#' and the loaded data, each criterion read as the page reads it
#' (`sieve_criterion_values()`: a factor by its labels, never by its level
#' codes) and each interval at the page's confidence level.
#'
#' @param criteria Names of the criterion variables.
#' @param method The score method label from the module.
#' @param stated The rows of the evidence table that carry a prediction
#'   (`construct`, `criterion`, `kind`, `sign`, `min_abs`, `bound`).
#' @param elsewhere Stated predictions for pairs the table does not show
#'   (see `sieve_predictions_elsewhere()`), or `NULL`. They are written too,
#'   so that "none" in the first list is never read as "none were stated".
#' @param design `NULL` for independent rows, or the sentence that says why
#'   the declared design supports no interval.
#' @param score_code Optional R lines that form `construct_scores` (one
#'   column per construct) under the operational score specification
#'   (`sieve_score_specification_code()`); when given, they
#'   replace the mean-score lines and `method` is only described.
#' @param conf_level The confidence level of the Fisher z intervals, the
#'   one the table was computed at, written as `conf.level` in every
#'   recorded `stats::cor.test()` call.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_validity_code <- function(criteria, method, stated, elsewhere = NULL, design = NULL,
                                score_code = NULL, conf_level = sieve_default_conf_level()) {
  conf_level <- sieve_check_conf_level(conf_level)
  level <- sieve_num_lit(conf_level)
  # Each criterion read as sieve_criterion_values() reads it, so a replay
  # correlates the values the table correlated.
  reader <- c(
    "# Each criterion read as SIEVER reads it: a factor by its labels, never by its",
    "# level codes; numerals stored as text as those numbers; a blank entry as",
    "# missing. An entry that is not a number stops the script.",
    "criterion_values <- function(v) {",
    "  if (is.factor(v)) v <- as.character(v)",
    "  if (!is.character(v)) return(as.numeric(unclass(v)))",
    "  v[!is.na(v) & !nzchar(trimws(v))] <- NA",
    "  num <- suppressWarnings(as.numeric(v))",
    "  lost <- v[!is.na(v) & is.na(num)]",
    "  if (length(lost)) stop(\"A criterion holds entries that are not numbers (\", length(lost), \" of them, for example \\\"\",",
    "    lost[1], \"\\\"); score or recode it as numbers before correlating.\", call. = FALSE)",
    "  num",
    "}"
  )
  chr <- function(x) sprintf("c(%s)", paste(encodeString(as.character(x), quote = "\""), collapse = ", "))
  listed <- function(rows) {
    sprintf(
      "#   %s with %s: %s", rows$construct, rows$criterion,
      unname(mapply(
        sieve_validity_prediction_text, rows$kind, rows$sign,
        rows$min_abs, rows$bound
      ))
    )
  }
  # The stated pairs as data, with every number an exact literal.
  as_data <- function(rows) {
    num <- function(v) paste(sieve_num_lit(as.numeric(v)), collapse = ", ")
    c(
      "predictions <- data.frame(",
      sprintf("  construct = %s,", chr(rows$construct)),
      sprintf("  criterion = %s,", chr(rows$criterion)),
      sprintf("  kind = %s,", chr(rows$kind)),
      sprintf("  sign = c(%s),", num(rows$sign)),
      sprintf("  min_abs = c(%s),", num(rows$min_abs)),
      sprintf("  bound = c(%s)", num(rows$bound)),
      ")"
    )
  }
  predictions <- c(
    "# Stated predictions (the researcher's own; the code does not compute them):",
    if (!is.null(stated) && nrow(stated)) c(listed(stated), as_data(stated)) else "#   none",
    "# A pair without a stated prediction is reported and not scored.",
    if (!is.null(elsewhere) && nrow(elsewhere)) {
      c(
        "# Also stated, for pairs that are not in this table (kept, and applied when",
        "# the pair is shown again):",
        listed(elsewhere)
      )
    }
  )
  design_note <- if (!is.null(design)) {
    c(
      sieve_comment_lines("Sampling design:", design),
      "# The correlations are therefore descriptive: no interval and no p-value."
    )
  }
  if (!is.null(score_code)) {
    # The scores as specified per construct, then the correlation of each
    # column with each criterion.
    return(c(
      predictions,
      design_note,
      sprintf("# Construct scores: %s.", method),
      score_code,
      reader,
      sprintf("criterion_r <- do.call(rbind, lapply(%s, function(cr) {", chr(criteria)),
      "  y <- criterion_values(d[[cr]])",
      "  do.call(rbind, lapply(colnames(construct_scores), function(cn) {",
      if (is.null(design)) {
        c(
          sprintf("    ct <- stats::cor.test(construct_scores[[cn]], y, conf.level = %s)", level),
          "    ci <- if (is.null(ct$conf.int)) c(NA_real_, NA_real_) else ct$conf.int",
          "    data.frame(construct = cn, criterion = cr, r = unname(ct$estimate),",
          "               lower = ci[1], upper = ci[2], n = sum(is.finite(construct_scores[[cn]]) & is.finite(y)))"
        )
      } else {
        c(
          "    data.frame(construct = cn, criterion = cr,",
          "               r = stats::cor(construct_scores[[cn]], y, use = \"complete.obs\"))"
        )
      },
      "  }))",
      "}))"
    ))
  }
  if (!startsWith(method, "mean score")) {
    return(c(
      predictions,
      design_note,
      "# Bartlett factor scores from the fitted measurement model were correlated with",
      if (is.null(design)) {
        c(
          sprintf("# each criterion (Pearson, %s Fisher z interval), one construct at a time,", sieve_ci_percent(conf_level)),
          "# a factor criterion read by its labels, never by its level codes:",
          sprintf("#   stats::cor.test(factor_score, criterion, conf.level = %s)", level)
        )
      } else {
        c(
          "# each criterion (Pearson, descriptive), one construct at a time:",
          "#   stats::cor(factor_score, criterion, use = \"complete.obs\")"
        )
      }
    ))
  }
  if (!is.null(design)) {
    return(c(
      predictions,
      design_note,
      "# Mean score of each construct's included items (a case missing some items",
      "# still contributes), correlated with each criterion.",
      "included <- meta[meta$status == \"active\", ]",
      "construct_scores <- sapply(",
      "  split(included$item, factor(included$construct, levels = unique(included$construct))),",
      "  function(its) rowMeans(data.matrix(items[, its, drop = FALSE]), na.rm = TRUE))",
      reader,
      sprintf("criterion_r <- do.call(rbind, lapply(%s, function(cr) {", chr(criteria)),
      "  y <- criterion_values(d[[cr]])",
      "  do.call(rbind, lapply(colnames(construct_scores), function(cn) {",
      "    data.frame(construct = cn, criterion = cr,",
      "               r = stats::cor(construct_scores[, cn], y, use = \"complete.obs\"))",
      "  }))",
      "}))"
    ))
  }
  c(
    predictions,
    "# Mean score of each construct's included items (a case missing some items",
    "# still contributes), correlated with each criterion.",
    "included <- meta[meta$status == \"active\", ]",
    "construct_scores <- sapply(",
    "  split(included$item, factor(included$construct, levels = unique(included$construct))),",
    "  function(its) rowMeans(data.matrix(items[, its, drop = FALSE]), na.rm = TRUE))",
    reader,
    sprintf("criterion_r <- do.call(rbind, lapply(%s, function(cr) {", chr(criteria)),
    "  y <- criterion_values(d[[cr]])",
    "  do.call(rbind, lapply(colnames(construct_scores), function(cn) {",
    sprintf("    ct <- stats::cor.test(construct_scores[, cn], y, conf.level = %s)", level),
    "    ci <- if (is.null(ct$conf.int)) c(NA_real_, NA_real_) else ct$conf.int",
    "    data.frame(construct = cn, criterion = cr, r = unname(ct$estimate),",
    "               lower = ci[1], upper = ci[2])",
    "  }))",
    "}))"
  )
}


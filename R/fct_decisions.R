# fct_decisions.R
# Decision records. Scale development turns on a small number of choices
# that the data inform and the researcher makes: how many factors to retain,
# which items to drop, which model to carry forward. An analysis can be
# rerun from its recorded call; a choice can only be reconstructed if it was
# written down with its reasons when it was made. A decision record keeps
# the question, the choice, the researcher's own rationale, what was looked
# at, and the inputs in force at that moment. Records are appended and never
# edited: a changed mind is a new record that names the one it replaces, so
# the history of a decision stays readable (Flake and Fried, 2020, on
# transparent measurement practice).

#' Record a researcher's decision
#'
#' Appends one decision record to `state$decisions` and writes it into the
#' reproducible code as a comment block. The environment never fills in a
#' decision: the choice and the rationale are the researcher's. A rationale
#' is welcome and not required: a
#' record made without one carries the words `sieve_no_rationale()` in its
#' `rationale` field, so the record, the table and the reproducible code
#' say that none was given rather than leaving the field blank.
#'
#' Records are never changed in place. A new record on a `topic` that
#' already has a record for the loaded dataset names that earlier record in
#' `supersedes`, and the earlier record stays in the list as it was written.
#'
#' Before a record is appended, the earlier records are compared with the
#' keys written for them in the reproducible code (see
#' `sieve_decisions_check()`). When one of them no longer matches its key,
#' it was changed after it was recorded, and nothing is appended: an error
#' of class `sieve_decisions_altered` names the records. A chain of records
#' is extended only while every link in it is what was written.
#'
#' @param state The shared state store.
#' @param topic A short identifier of what is being decided, for example
#'   `"factor_retention"`. Records with the same topic on the same dataset
#'   form one chain of revisions.
#' @param question The question the decision answers, in plain words.
#' @param choice The choice made: a single number or string, stored as given.
#' @param rationale The researcher's reason, optional. `NULL`, a missing
#'   argument, or text without a single letter or digit (white space of any
#'   kind, or punctuation alone; see `sieve_has_content()`) is recorded as
#'   `sieve_no_rationale()`. Anything else must be a single character
#'   string, which is trimmed (`sieve_trim()`).
#' @param evidence Optional character vector: identifiers of the analyses,
#'   or a short description of what was looked at.
#' @return The record, invisibly: a plain list with `id` (`"D1"`, `"D2"`,
#'   and so on), `topic`, `question`, `choice`, `rationale`, `evidence`,
#'   `supersedes` (an earlier id, or `NA`), `revision` (from
#'   `sieve_revision()`), `version` (the SIEVER version), and `time` (UTC,
#'   ISO 8601).
#' @author Ken Kelley
#' @noRd
sieve_record_decision <- function(state, topic, question, choice, rationale = NULL,
                                  evidence = NULL) {
  one_string <- function(x, what) {
    ok <- is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
    if (!ok) {
      stop(sprintf("`%s` must be a single character string that is not empty.", what),
        call. = FALSE
      )
    }
    trimws(x)
  }
  topic <- one_string(topic, "topic")
  question <- one_string(question, "question")
  # A rationale that says nothing (absent, empty, white space of any kind,
  # punctuation alone) is recorded as such; a value that is not text at all
  # is a programming error and is refused.
  if (!is.null(rationale) && !is.character(rationale)) {
    stop("`rationale` must be a single character string, or NULL when none is given.",
      call. = FALSE
    )
  }
  if (!is.null(rationale) && length(rationale) > 1L) {
    stop("`rationale` must be a single character string, or NULL when none is given.",
      call. = FALSE
    )
  }
  rationale <- if (sieve_has_content(rationale)) sieve_trim(rationale) else sieve_no_rationale()
  if (is.null(choice) || length(choice) != 1L || is.list(choice) || is.na(choice) ||
    (is.character(choice) && !nzchar(trimws(choice)))) {
    stop("`choice` must be a single number or character string that is not missing.",
      call. = FALSE
    )
  }
  evidence <- if (is.null(evidence)) character(0) else as.character(unlist(evidence))
  evidence <- trimws(evidence[!is.na(evidence)])
  evidence <- evidence[nzchar(evidence)]

  existing <- shiny::isolate(state$decisions) %||% list()
  # The earlier records are verified against the keys written for them
  # when they were made. An edited record is refused as a base for a new
  # one, so that the chain in the script and the chain in the session
  # cannot drift apart without a word.
  check <- shiny::isolate(sieve_decisions_check(state))
  altered <- check$id[check$intact %in% FALSE]
  if (length(altered)) {
    stop(structure(
      class = c("sieve_decisions_altered", "error", "condition"),
      list(
        message = sprintf(
          paste0(
            "Decision record%s %s no longer match%s the key written to the reproducible code ",
            "when %s recorded, so %s changed afterwards. Decision records are appended and ",
            "never edited; nothing was recorded."
          ),
          if (length(altered) == 1L) "" else "s", paste(altered, collapse = ", "),
          if (length(altered) == 1L) "es" else "",
          if (length(altered) == 1L) "it was" else "they were",
          if (length(altered) == 1L) "it was" else "they were"
        ),
        call = NULL,
        altered = altered
      )
    ))
  }
  revision <- sieve_revision(state)
  prior <- sieve_current_decision(existing, topic, revision$dataset_id)
  id <- sprintf("D%d", length(existing) + 1L)
  record <- list(
    id = id,
    topic = topic,
    question = question,
    choice = choice,
    rationale = rationale,
    evidence = evidence,
    supersedes = if (is.null(prior)) NA_character_ else prior$id,
    revision = revision,
    version = sieve_version(),
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  state$decisions <- c(existing, stats::setNames(list(record), id))
  sieve_log(
    state,
    sprintf(
      "Decision %s (%s): %s%s", id, topic, format(choice),
      if (is.null(prior)) "" else sprintf(", replacing %s", prior$id)
    ),
    sieve_decision_code(record)
  )
  invisible(record)
}

#' The words a decision record carries when no rationale was given
#'
#' One string, used by `sieve_record_decision()` when the rationale is
#' absent or says nothing, and read by the displays that distinguish a
#' stated rationale from none.
#'
#' @return `"no rationale given"`.
#' @author Ken Kelley
#' @noRd
sieve_no_rationale <- function() {
  "no rationale given"
}

#' Code points that are blank on the page
#'
#' White space and invisible characters as Unicode code points: the ASCII
#' controls and the space, the next line control, the non-breaking space,
#' the Ogham and Mongolian separators, the spaces, zero width characters and
#' direction marks of the General Punctuation block, the line and paragraph
#' separators, the narrow and medium mathematical spaces, the word joiner,
#' the ideographic space, and the byte order mark. Working with code points
#' keeps the test the same in every locale.
#'
#' @return An integer vector of code points.
#' @author Ken Kelley
#' @noRd
sieve_blank_points <- function() {
  c(
    0:32, 127L, 133L, 160L, 173L, 5760L, 6158L, 8192:8207, 8232:8239, 8287:8292,
    12288L, 65279L
  )
}

#' The code points of a string
#'
#' Text from the browser arrives as UTF-8 and is read as such, whatever the
#' session's locale; a string declared Latin-1, or one that is not valid
#' UTF-8, is converted first.
#'
#' @param x A single character string.
#' @return An integer vector, or `NULL` when the string cannot be read as
#'   text in either way.
#' @author Ken Kelley
#' @noRd
sieve_code_points <- function(x) {
  read <- function(s) tryCatch(utf8ToInt(s), error = function(e) NA_integer_)
  points <- if (identical(Encoding(x), "latin1")) NA_integer_ else read(x)
  if (anyNA(points)) {
    points <- read(enc2utf8(x))
  }
  if (anyNA(points)) NULL else points
}

#' Does a text say anything?
#'
#' The test behind a required free-text field. Blank is not only the empty
#' string and ordinary white space: a non-breaking space, the other Unicode
#' space characters, and punctuation alone carry no reasoning either. A
#' text has content when at least one of its characters is neither blank
#' (see `sieve_blank_points()`) nor punctuation (the ASCII and Latin-1
#' punctuation and symbols, and the General Punctuation block). Letters and
#' digits of any script therefore count. The test cannot tell a good reason
#' from a poor one, and is not meant to.
#'
#' @param x A value from a text input.
#' @return `TRUE` for a single, non-missing character string with content;
#'   `FALSE` otherwise.
#' @author Ken Kelley
#' @noRd
sieve_has_content <- function(x) {
  if (is.null(x) || !is.character(x) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  points <- sieve_code_points(x)
  if (is.null(points)) {
    return(grepl("[[:alnum:]]", x, useBytes = TRUE))
  }
  punctuation <- c(33:47, 58:64, 91:96, 123:126, 161:191, 215L, 247L, 8208:8303)
  length(setdiff(points, c(sieve_blank_points(), punctuation))) > 0L
}

#' Trim white space of every kind from both ends of a string
#'
#' [trimws()] with its default pattern leaves a non-breaking space and the
#' other Unicode space characters in place; this removes them as well (see
#' `sieve_blank_points()`). Characters inside the text are left as they are.
#'
#' @param x A single character string.
#' @return `x`, trimmed.
#' @author Ken Kelley
#' @noRd
sieve_trim <- function(x) {
  points <- sieve_code_points(x)
  if (is.null(points)) {
    return(trimws(x))
  }
  keep <- which(!points %in% sieve_blank_points())
  if (!length(keep)) {
    return("")
  }
  intToUtf8(points[keep[1]:keep[length(keep)]])
}

#' The key of a decision record
#'
#' A content hash of everything the record holds. It is written into the
#' reproducible code with the record, so the script carries what the record
#' was when it was made: a record changed afterwards no longer has that
#' key (see `sieve_decisions_check()`). The key identifies content; it is
#' not a security measure.
#'
#' @param record A decision record from `sieve_record_decision()`.
#' @return A string of 32 hexadecimal digits.
#' @author Ken Kelley
#' @noRd
sieve_decision_key <- function(record) {
  fields <- c(
    "id", "topic", "question", "choice", "rationale", "evidence",
    "supersedes", "revision", "version", "time"
  )
  sieve_hash(unname(record[fields]))
}

#' Are the decision records still what was recorded?
#'
#' Decision records are appended and never edited. Nothing in R prevents an
#' assignment into the list that holds them, so the rule is made checkable:
#' each record's key (see `sieve_decision_key()`) was written to the
#' reproducible code when the record was made, and this compares the key of
#' each record as it is now with the key in the code.
#'
#' @param state The shared state store (its records are read as they are,
#'   so inside a reactive this takes a dependency on them; the code log is
#'   read in isolation).
#' @return A data frame with `id`, `key` (of the record as it is now),
#'   `recorded_key` (from the reproducible code, or `NA` when the code
#'   holds none for that record), and `intact` (`TRUE`, `FALSE`, or `NA`
#'   when there is nothing to compare with).
#' @author Ken Kelley
#' @noRd
sieve_decisions_check <- function(state) {
  records <- sieve_decision_records(state)
  log <- shiny::isolate(state$codelog) %||% character(0)
  ids <- vapply(records, function(r) as.character(r$id %||% NA_character_), character(1),
    USE.NAMES = FALSE
  )
  now <- vapply(records, sieve_decision_key, character(1), USE.NAMES = FALSE)
  written <- vapply(ids, function(id) {
    at <- which(startsWith(log, sprintf("# ---- Decision %s:", id)))
    if (!length(at)) {
      return(NA_character_)
    }
    # The key line of the block that starts there (the block ends at the
    # first line that is not a comment).
    after <- log[at[length(at)]:length(log)]
    stop_at <- which(!startsWith(after, "#"))[1]
    block <- if (is.na(stop_at)) after else after[seq_len(stop_at - 1L)]
    line <- block[startsWith(block, "# Record key:")]
    if (length(line)) sub("^# Record key: +", "", line[1]) else NA_character_
  }, character(1), USE.NAMES = FALSE)
  data.frame(
    id = ids, key = now, recorded_key = written,
    intact = ifelse(is.na(written), NA, now == written),
    stringsAsFactors = FALSE
  )
}

#' The decision currently in force on a topic
#'
#' The most recent record with the given topic for the given dataset. Later
#' records replace earlier ones, so the last one in the list is the one that
#' has not been superseded.
#'
#' @param decisions A list of decision records, or the shared state store
#'   (read as it is: inside a reactive this takes a dependency on the
#'   records, and outside one the caller wraps the call in
#'   [shiny::isolate()]).
#' @param topic The topic identifier.
#' @param dataset_id The dataset the decision must belong to. The default,
#'   `NULL`, accepts any dataset.
#' @return A decision record, or `NULL` when there is none.
#' @author Ken Kelley
#' @noRd
sieve_current_decision <- function(decisions, topic, dataset_id = NULL) {
  records <- sieve_decision_records(decisions)
  for (rec in rev(records)) {
    same_data <- is.null(dataset_id) ||
      identical(rec$load_dataset_id %||% rec$revision$dataset_id, dataset_id)
    if (identical(rec$topic, topic) && same_data) {
      return(rec)
    }
  }
  NULL
}

#' The list of decision records behind a state store or a list
#'
#' @param x The shared state store, a list with a `decisions` element, or a
#'   list of records.
#' @return A list of records, possibly empty.
#' @author Ken Kelley
#' @noRd
sieve_decision_records <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (shiny::is.reactivevalues(x)) {
    return(x$decisions %||% list())
  }
  if (is.list(x) && "decisions" %in% names(x)) {
    return(x[["decisions"]] %||% list())
  }
  x
}

#' The decision records as a table
#'
#' One row per record, oldest first, for display and for export. Nothing is
#' rounded or abbreviated: the table is the records, flattened. Whether a
#' record is still in force is derived here, from the later records that
#' name it, and is not stored in the record itself.
#'
#' @param state The shared state store, or a list of decision records (see
#'   `sieve_current_decision()` for how a state store is read).
#' @return A data frame with character columns `id`, `topic`, `question`,
#'   `choice`, `rationale`, `evidence` (entries joined by `"; "`),
#'   `supersedes`, `superseded_by`, `status` (`"current"` or
#'   `"superseded"`), `dataset_id`, `data_key`, `meta_key`,
#'   `siever_version`, and `recorded_at`. Zero rows when nothing has been
#'   recorded.
#' @author Ken Kelley
#' @noRd
sieve_decisions_table <- function(state) {
  records <- sieve_decision_records(state)
  field <- function(get) {
    vapply(records, function(r) {
      v <- get(r)
      if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1]
    }, character(1), USE.NAMES = FALSE)
  }
  out <- data.frame(
    id = field(function(r) r$id),
    topic = field(function(r) r$topic),
    question = field(function(r) r$question),
    choice = field(function(r) format(r$choice, digits = 15)),
    rationale = field(function(r) r$rationale),
    evidence = field(function(r) if (length(r$evidence)) paste(r$evidence, collapse = "; ") else NA_character_),
    supersedes = field(function(r) r$supersedes),
    stringsAsFactors = FALSE
  )
  out$superseded_by <- vapply(out$id, function(i) {
    later <- out$id[!is.na(out$supersedes) & out$supersedes == i]
    if (length(later)) later[length(later)] else NA_character_
  }, character(1), USE.NAMES = FALSE)
  out$status <- ifelse(is.na(out$superseded_by), "current", "superseded")
  out$dataset_id <- field(function(r) r$revision$dataset_id)
  out$data_key <- field(function(r) r$revision$data_key)
  out$meta_key <- field(function(r) r$revision$meta_key)
  out$siever_version <- field(function(r) r$version)
  out$recorded_at <- field(function(r) r$time)
  out
}

#' Free text as comment lines
#'
#' Researcher-entered text goes into the reproducible script, so every line
#' of it must be a comment whatever it contains: line breaks inside the text
#' start new comment lines and never a line of code.
#'
#' @param label A short label for the first line, for example
#'   `"Rationale:"`.
#' @param text A character vector of text.
#' @param width Wrapping width of the text.
#' @return A character vector of lines, each starting with `"#"`.
#' @author Ken Kelley
#' @noRd
sieve_comment_lines <- function(label, text, width = 66L) {
  text <- as.character(text)
  text <- unlist(strsplit(text, "\r\n|\r|\n"))
  text <- trimws(text[!is.na(text)])
  text <- text[nzchar(text)]
  head <- formatC(label, width = -11L, flag = "-")
  if (!length(text)) {
    return(sprintf("# %s (none)", head))
  }
  body <- unlist(lapply(text, strwrap, width = width))
  paste0("# ", c(head, rep(strrep(" ", 11L), length(body) - 1L)), " ", body)
}

#' A decision record as a comment block for the reproducible code
#'
#' @param record A decision record from `sieve_record_decision()`.
#' @return A character vector of comment lines.
#' @author Ken Kelley
#' @noRd
sieve_decision_code <- function(record) {
  rev <- record$revision
  short <- function(key) if (is.na(key)) "not available" else substr(key, 1L, 12L)
  c(
    sprintf("# ---- Decision %s: %s", record$id, record$topic),
    "# A decision made by the researcher. It is not computed by the code.",
    sieve_comment_lines("Question:", record$question),
    sieve_comment_lines("Choice:", format(record$choice, digits = 15)),
    sieve_comment_lines("Rationale:", record$rationale),
    sieve_comment_lines("Evidence:", record$evidence),
    if (!is.na(record$supersedes)) {
      sieve_comment_lines("Replaces:", sprintf(
        "%s, which stays in the record as it was written.",
        record$supersedes
      ))
    },
    sieve_comment_lines("Inputs:", sprintf(
      "dataset %s; data key %s; ledger key %s",
      if (is.na(rev$dataset_id)) "not identified" else rev$dataset_id,
      short(rev$data_key), short(rev$meta_key)
    )),
    sieve_comment_lines("Recorded:", sprintf("%s with SIEVER %s", record$time, record$version)),
    # The key of the record as it was made; a record edited afterwards no
    # longer has it (sieve_decisions_check).
    sieve_comment_lines("Record key:", sieve_decision_key(record))
  )
}

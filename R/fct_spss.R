# fct_spss.R
# The SPSS measurement level of each variable of a .sav file. haven reads
# the values, the variable labels and the value labels but not the level
# (Nominal, Ordinal, Scale) the researcher set in SPSS's Variable View. The
# level is in the file's dictionary, in the variable display record (record
# type 7, subtype 11: per variable, the measure, 1 = Nominal, 2 = Ordinal,
# 3 = Scale, then the display width and the alignment). The Ingest tab reads
# it so that it can say when SPSS calls an item ordinal while SIEVER treats
# it as continuous (five or more response options).

#' Read the dictionary of an SPSS .sav file
#'
#' Walks the dictionary records of a system file (little or big endian) up
#' to the end of the dictionary, without reading the data: the variable
#' records (type 2; continuation records of long strings, type -1, are
#' dropped), the value label records (types 3 and 4) and the document
#' record (type 6) are skipped over, and the extension records (type 7) are
#' kept by subtype. The long variable names (subtype 13) replace the
#' eight-byte short names, and the segments of very long strings (subtype
#' 14) are collapsed, so that the rows align with the columns
#' [haven::read_sav()] returns.
#'
#' @param path The .sav file.
#' @return A data frame with one row per variable: `name`, `short`, `type`
#'   (0 numeric, otherwise the string width), `measure` (1, 2, 3, or `NA`
#'   when the file has no display record), `measure_label` (`"Nominal"`,
#'   `"Ordinal"`, `"Scale"`, or `NA`), and `missing` (a list column: for a
#'   numeric variable with user-missing values, a list with `values`, the
#'   discrete codes, and `range`, the low and high codes of a missing range
#'   or `NULL`; otherwise `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_spss_dictionary <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  magic <- rawToChar(readBin(con, "raw", 4L))
  if (!magic %in% c("$FL2", "$FL3")) {
    stop("not an SPSS system file")
  }
  invisible(readBin(con, "raw", 60L))
  layout <- readBin(con, "raw", 4L)
  # The layout code is 2 or 3; its byte order gives the file's.
  endian <- if (readBin(layout, "integer", size = 4L, endian = "little") %in% c(2L, 3L)) "little" else "big"
  ri <- function(n = 1L) readBin(con, "integer", n, size = 4L, endian = endian)
  rraw <- function(n) readBin(con, "raw", n)
  invisible(ri(4L)) # case size, compression, weight index, cases
  invisible(rraw(8L + 9L + 8L + 64L + 3L)) # bias, creation date and time, file label, padding
  vars <- list()
  sub <- list()
  rdbl <- function(n) readBin(con, "double", n, size = 8L, endian = endian)
  repeat {
    rt <- ri()
    if (!length(rt)) {
      stop("the dictionary ended early")
    }
    if (rt == 2L) {
      type <- ri()
      has_label <- ri()
      n_missing <- ri()
      invisible(ri(2L))
      short <- trimws(rawToChar(rraw(8L)))
      if (has_label == 1L) {
        len <- ri()
        invisible(rraw(4L * ceiling(len / 4)))
      }
      missing <- NULL
      if (n_missing != 0L) {
        codes <- rdbl(abs(n_missing))
        # A numeric variable's user-missing values: up to three discrete
        # codes, or a range (n_missing -2) and one more code (-3).
        if (type == 0L) {
          missing <- if (n_missing < 0L) {
            list(range = codes[1:2], values = codes[-(1:2)])
          } else {
            list(range = NULL, values = codes)
          }
        }
      }
      vars[[length(vars) + 1L]] <- list(short = short, type = type, missing = missing)
    } else if (rt == 3L) {
      n_labels <- ri()
      for (i in seq_len(n_labels)) {
        invisible(rraw(8L))
        len <- as.integer(rraw(1L))
        invisible(rraw(8L * ceiling((len + 1) / 8) - 1L))
      }
      if (ri() != 4L) {
        stop("a value label record is not followed by its variable index record")
      }
      invisible(ri(ri()))
    } else if (rt == 6L) {
      invisible(rraw(80L * ri()))
    } else if (rt == 7L) {
      subtype <- ri()
      size <- ri()
      count <- ri()
      sub[[as.character(subtype)]] <- list(count = count, bytes = rraw(size * count))
    } else if (rt == 999L) {
      break
    } else {
      stop(sprintf("record type %d is not a dictionary record", rt))
    }
  }
  type <- vapply(vars, function(v) v$type, integer(1))
  short <- vapply(vars, function(v) v$short, character(1))
  keep <- type != -1L
  missing_codes <- lapply(vars, function(v) v$missing)[keep]
  dict <- data.frame(short = short[keep], type = type[keep], stringsAsFactors = FALSE)
  dict$missing <- missing_codes
  dict$name <- dict$short
  if (!is.null(sub[["13"]])) {
    pairs <- strsplit(strsplit(rawToChar(sub[["13"]]$bytes), "\t", fixed = TRUE)[[1]], "=", fixed = TRUE)
    long <- stats::setNames(vapply(pairs, function(p) p[2], ""), vapply(pairs, function(p) p[1], ""))
    hit <- dict$short %in% names(long)
    dict$name[hit] <- unname(long[dict$short[hit]])
  }
  dict$measure <- NA_integer_
  s11 <- sub[["11"]]
  if (!is.null(s11)) {
    display <- readBin(s11$bytes, "integer", s11$count, size = 4L, endian = endian)
    per <- if (s11$count == 3L * nrow(dict)) 3L else if (s11$count == 2L * nrow(dict)) 2L else NA_integer_
    if (!is.na(per)) {
      dict$measure <- matrix(display, ncol = per, byrow = TRUE)[, 1]
    }
  }
  # Very long strings are written as several segments; only the first is a
  # variable haven returns.
  if (!is.null(sub[["14"]])) {
    text <- rawToChar(sub[["14"]]$bytes[sub[["14"]]$bytes != as.raw(0)])
    entries <- Filter(nzchar, trimws(strsplit(text, "\t", fixed = TRUE)[[1]]))
    drop <- integer(0)
    for (e in entries) {
      kv <- strsplit(e, "=", fixed = TRUE)[[1]]
      i <- match(kv[1], dict$short)
      segments <- ceiling(as.integer(kv[2]) / 252)
      if (!is.na(i) && segments > 1) {
        drop <- c(drop, i + seq_len(segments - 1L))
      }
    }
    if (length(drop)) {
      dict <- dict[-drop, , drop = FALSE]
    }
  }
  dict$measure_label <- unname(c("1" = "Nominal", "2" = "Ordinal", "3" = "Scale")[as.character(dict$measure)])
  rownames(dict) <- NULL
  dict[, c("name", "short", "type", "measure", "measure_label", "missing")]
}

#' The SPSS measurement level of each variable of a .sav file
#'
#' @param path The .sav file.
#' @return A named character vector (`"Nominal"`, `"Ordinal"`, `"Scale"`,
#'   or `NA`), named by variable; `NULL` when the dictionary cannot be read
#'   or holds no display record.
#' @author Ken Kelley
#' @noRd
sieve_spss_measures <- function(path) {
  dict <- tryCatch(sieve_spss_dictionary(path), error = function(e) NULL)
  if (is.null(dict) || all(is.na(dict$measure_label))) {
    return(NULL)
  }
  stats::setNames(dict$measure_label, dict$name)
}

#' The user-missing codes of each numeric variable of a .sav file
#'
#' haven maps user-missing values to `NA` but keeps their value labels, so
#' the codes are read from the dictionary to keep them out of the response
#' options the labels declare (see `sieve_label_bounds()`).
#'
#' @param path The .sav file.
#' @return A named list (by variable) of `list(values, range)`, only for
#'   the variables that declare user-missing values; `NULL` when the
#'   dictionary cannot be read.
#' @author Ken Kelley
#' @noRd
sieve_spss_missing <- function(path) {
  dict <- tryCatch(sieve_spss_dictionary(path), error = function(e) NULL)
  if (is.null(dict)) {
    return(NULL)
  }
  out <- stats::setNames(dict$missing, dict$name)
  Filter(Negate(is.null), out)
}

#' Mark each column of a data frame with its SPSS measurement level
#'
#' The level travels with the column as the attribute `spss_measure`, as
#' the variable label does, so the ledger built from the columns can carry
#' it (see `sieve_build_meta()`).
#'
#' @param df A data frame read from the .sav file.
#' @param measures The result of `sieve_spss_measures()`.
#' @param missing The result of `sieve_spss_missing()`, kept on each column
#'   as the attribute `spss_missing`.
#' @return `df` with the attributes set on every column that has them.
#' @author Ken Kelley
#' @noRd
sieve_spss_mark <- function(df, measures, missing = NULL) {
  for (v in intersect(names(df), names(missing))) {
    attr(df[[v]], "spss_missing") <- missing[[v]]
  }
  if (is.null(measures)) {
    return(df)
  }
  for (v in intersect(names(df), names(measures))) {
    if (!is.na(measures[[v]])) {
      attr(df[[v]], "spss_measure") <- measures[[v]]
    }
  }
  df
}

#' Items SPSS calls ordinal or nominal that SIEVER treats as continuous
#'
#' Each item comes with the reason it is typed continuous, which is not
#' always the five-option rule: `"options"` (five or
#' more response options, the rule), `"chosen"` (the researcher typed it
#' continuous), or `"values"` (its values are not whole numbers), and
#' `"typed"` when the item data are not given or none of these applies.
#'
#' @param meta The item ledger (with its `spss_measure` column when the
#'   data came from a .sav file).
#' @param items The item data, or `NULL`.
#' @return A data frame with `item`, `label`, `spss_measure`, `options`
#'   (the response options, `NA` without the item data) and `reason` for
#'   each included item typed continuous whose SPSS level is Ordinal or
#'   Nominal; no rows when there is none or the ledger carries no SPSS
#'   level.
#' @author Ken Kelley
#' @noRd
sieve_spss_level_conflicts <- function(meta, items = NULL) {
  empty <- data.frame(
    item = character(0), label = character(0), spss_measure = character(0),
    options = integer(0), reason = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(meta) || !"spss_measure" %in% names(meta)) {
    return(empty)
  }
  hit <- meta$status == "active" & meta$type %in% "continuous" &
    meta$spss_measure %in% c("Ordinal", "Nominal")
  if (!any(hit)) {
    return(empty)
  }
  its <- meta$item[hit]
  chosen <- sieve_type_source(meta)[hit] == "chosen"
  options <- rep(NA_integer_, length(its))
  reason <- ifelse(chosen, "chosen", "typed")
  if (!is.null(items)) {
    tab <- sieve_item_options(items, meta)
    options <- tab$options[match(its, tab$item)]
    whole <- vapply(its, function(it) {
      v <- sieve_as_number(items[[it]])
      v <- v[is.finite(v)]
      !length(v) || all(abs(v - round(v)) < .Machine$double.eps^0.5)
    }, logical(1), USE.NAMES = FALSE)
    reason[!chosen & !whole] <- "values"
    reason[!chosen & whole & options >= 5L] <- "options"
  }
  data.frame(
    item = its, label = as.character(meta$label[hit]),
    spss_measure = meta$spss_measure[hit], options = as.integer(options), reason = reason,
    stringsAsFactors = FALSE
  )
}

# Import.

# ---- Reading delimited text the same way in every locale --------------------
#
# A comma or tab separated file is bytes, and R reads it through the
# session's character locale: in a session whose locale is not UTF-8 (a C
# or POSIX locale under cron, in a minimal container, or R 4.1 on Windows)
# read.csv() turns a column name such as qualite with an accent into
# "qualit..", stops at the first character the locale cannot represent (in
# a C locale, any outside ASCII; in a single byte locale such as Latin-1,
# any outside its 256) and returns the rows before it with only a warning,
# and keeps an Excel byte order mark in the first column's name. The
# readers below read the bytes first, check them against the stated
# encoding, read a file that holds characters outside ASCII only in a UTF-8
# session (the one locale that represents every character, so the file and
# the session script that replays its read give the same data frame on
# every machine), refuse otherwise with that rule, and only then parse,
# from a copy of the text in UTF-8 that ends with a line end, so that any
# warning the parser gives means lost rows and is a refusal.

#' The encodings SIEVER reads delimited text in
#'
#' @return `c("UTF-8", "windows-1252", "latin1")`: UTF-8 (what Excel's "CSV
#'   UTF-8" and every SIEVER export write), windows-1252 (what Excel on
#'   Windows writes as "CSV (Comma delimited)"), and latin1 (ISO 8859-1).
#' @author Ken Kelley
#' @noRd
sieve_text_encodings <- function() {
  c("UTF-8", "windows-1252", "latin1")
}

#' The separators SIEVER reads delimited text with
#'
#' @return Comma, semicolon, tab and vertical bar, named in words.
#' @author Ken Kelley
#' @noRd
sieve_text_separators <- function() {
  c(comma = ",", semicolon = ";", tab = "\t", "vertical bar" = "|")
}

#' Check the parsing choices for a delimited file
#'
#' @param sep The separator, or `NULL` for the one the file type implies.
#' @param dec The decimal mark.
#' @param encoding The encoding (`""`, the session's own, is read as UTF-8,
#'   since a session that is not UTF-8 reads only ASCII files, which are
#'   UTF-8 as well).
#' @param ext The file type, `"csv"` or `"txt"`, for the default separator.
#' @return A list with `sep`, `dec` and `encoding`; stops with the reason
#'   otherwise.
#' @author Ken Kelley
#' @noRd
sieve_text_choices <- function(sep = NULL, dec = ".", encoding = "UTF-8", ext = "csv") {
  if (is.null(sep)) sep <- if (identical(ext, "txt")) "\t" else ","
  if (!is.character(sep) || length(sep) != 1L || is.na(sep) || !sep %in% sieve_text_separators()) {
    stop("`sep` must be one of \",\" (comma), \";\" (semicolon), \"\\t\" (tab) or \"|\" (vertical bar).", call. = FALSE)
  }
  if (!is.character(dec) || length(dec) != 1L || is.na(dec) || !dec %in% c(".", ",")) {
    stop("`dec` must be \".\" or \",\".", call. = FALSE)
  }
  if (identical(sep, dec)) {
    stop("The separator and the decimal mark must differ: a file whose decimals use commas is separated by semicolons (sep = \";\", dec = \",\").", call. = FALSE)
  }
  if (!is.character(encoding) || length(encoding) != 1L || is.na(encoding) ||
      !encoding %in% c(sieve_text_encodings(), "")) {
    stop(sprintf("`encoding` must be one of %s.", paste(sprintf("\"%s\"", sieve_text_encodings()), collapse = ", ")),
      call. = FALSE
    )
  }
  if (!nzchar(encoding)) encoding <- "UTF-8"
  list(sep = sep, dec = dec, encoding = encoding)
}

#' The first byte that is not part of a valid UTF-8 sequence
#'
#' Only the bytes outside ASCII are visited, from one to the next: every
#' byte of a multibyte sequence is outside ASCII, so a valid sequence of
#' `len` bytes is `len` consecutive entries of their positions, and an ASCII
#' byte is valid on its own. The check therefore takes as many steps as the
#' file has characters outside ASCII, not as it has bytes (walking byte by
#' byte through a windows-1252 file whose first accented letter is near its
#' end takes about 0.3 seconds a megabyte byte compiled, and every file read
#' in a single byte encoding is checked; see `sieve_text_bytes()`).
#'
#' @param bytes A raw vector.
#' @return Its position, or `NA` when every sequence is valid.
#' @author Ken Kelley
#' @noRd
sieve_utf8_first_invalid <- function(bytes) {
  b <- as.integer(bytes)
  n <- length(b)
  high <- which(b >= 0x80L)
  k <- 1L
  while (k <= length(high)) {
    i <- high[k]
    x <- b[i]
    len <- if (x >= 0xC2 && x <= 0xDF) 2L else if (x >= 0xE0 && x <= 0xEF) 3L else
      if (x >= 0xF0 && x <= 0xF4) 4L else 0L
    if (!len || i + len - 1L > n) return(i)
    cont <- b[i + seq_len(len - 1L)]
    if (any(cont < 0x80 | cont > 0xBF)) return(i)
    # Overlong three and four byte forms, surrogates, and code points
    # beyond U+10FFFF.
    if ((x == 0xE0 && cont[1] < 0xA0) || (x == 0xED && cont[1] > 0x9F) ||
        (x == 0xF0 && cont[1] < 0x90) || (x == 0xF4 && cont[1] > 0x8F)) return(i)
    k <- k + len
  }
  NA_integer_
}

#' Where a byte of a text file is, in words
#'
#' @param bytes The file's bytes (byte order mark removed).
#' @param at The byte's position.
#' @param sep The separator, to name the column; `NULL` names only the line.
#' @return `"line 3"`, `"line 150, column town"`, or `"line 1 (the header),
#'   column 2"`.
#' @author Ken Kelley
#' @noRd
sieve_text_place <- function(bytes, at, sep = NULL) {
  ends <- which(bytes == as.raw(0x0A))
  line <- sum(ends < at) + 1L
  where <- sprintf("line %d", line)
  if (is.null(sep)) return(where)
  from <- if (line == 1L) 1L else ends[line - 1L] + 1L
  piece <- as.integer(bytes[seq.int(from, at)])
  # The field the byte is in: separators outside double quotes before it.
  quoted <- cumsum(piece == 0x22L) %% 2L == 1L
  field <- sum(piece == utf8ToInt(sep) & !quoted) + 1L
  header_end <- if (length(ends)) ends[1] - 1L else length(bytes)
  header <- bytes[seq_len(header_end)]
  names <- NULL
  if (!any(header > as.raw(0x7F))) {
    names <- tryCatch(
      scan(text = rawToChar(header), what = "", sep = sep, quote = "\"", quiet = TRUE, strip.white = TRUE),
      error = function(e) NULL
    )
    names <- sub("\r$", "", names)
  }
  if (line == 1L) {
    return(sprintf("line 1 (the header), column %d", field))
  }
  if (length(names) >= field && nzchar(names[field])) {
    sprintf("%s, column %s", where, names[field])
  } else {
    sprintf("%s, column %d", where, field)
  }
}

#' Read a text file's bytes as UTF-8 text, or say why it cannot be read here
#'
#' The checks every delimited reader makes before parsing: the file is
#' text (no NUL byte, not UTF-16), its bytes are valid in the stated
#' encoding, and, when the R session's character locale is not UTF-8, it
#' holds only ASCII characters. SIEVER reads a file that holds characters
#' outside ASCII only in a UTF-8 session: a C or POSIX locale represents
#' none of them and a single byte locale such as Latin-1 or windows-1252
#' only some, and the session script's guard (`sieve_text_guard_code()`)
#' applies the same rule, so the script never reads differently from the
#' reader. A UTF-8 byte order mark (Excel's "CSV UTF-8") is removed in
#' every locale. The five bytes windows-1252 leaves undefined (0x81, 0x8D,
#' 0x8F, 0x90, 0x9D) are refused by this function itself, since the
#' platform's converter may map them to control characters (R for Windows
#' converts through the Windows code page tables) rather than fail.
#'
#' A file stated as windows-1252 or Latin-1 whose bytes are valid UTF-8 and
#' hold a character outside ASCII (at least one multibyte sequence) is
#' refused as UTF-8, with the first such character as the evidence, and is
#' never decoded in the single byte encoding: that would read each such
#' character as two to four others (an e with an acute accent as a capital
#' A with a tilde followed by a copyright sign), with no error, and the
#' session script would replay the same misreading. Text in windows-1252 or
#' Latin-1 that holds characters outside ASCII is almost never also valid
#' UTF-8 (see `utf8_first`), so a genuine windows-1252 file is read in
#' windows-1252. The rule matters most on the Ingest tab, whose Encoding
#' select applies to every data file and so carries a choice made for one
#' file over to the next; the byte order mark follows the same rule (a
#' file that begins with one is UTF-8 whatever is stated). The stated
#' encoding is refused rather than overridden, so the file is read in the
#' encoding the caller, the select and the session script name, or not at
#' all.
#'
#' A refusal says how to read the file another way through what the caller
#' offers: `argument` names the argument that states this file's encoding,
#' and `ingest` adds the Ingest tab's parsing choices, which read data
#' files only and are named only in the Ingest tab's own reads. A file that
#' is read only as UTF-8 (`argument = NULL`: a specification chosen on the
#' Ingest tab, a content ratings file) is told to be saved as CSV UTF-8.
#'
#' @param path The file.
#' @param encoding One of `sieve_text_encodings()`.
#' @param sep The separator, to name the column of a character in a
#'   refusal.
#' @param check_separator Whether to refuse a file whose header shows
#'   another separator (`sieve_text_separator_problem()`), before its
#'   encoding is checked, so the refusal names the first thing to change.
#' @param utf8_first Whether bytes that are valid UTF-8 (or begin with the
#'   UTF-8 byte order mark) are read as UTF-8 whatever `encoding` says, the
#'   stated encoding applying only to bytes that are not (a specification
#'   SIEVER wrote in UTF-8 beside a data file in windows-1252). Text in
#'   windows-1252 or Latin-1 that holds characters outside ASCII is almost
#'   never also valid UTF-8 (each such character would have to be one of
#'   the pairs a UTF-8 sequence forms, such as an accented capital followed
#'   directly by a curly quotation mark), so the rule tells the two apart.
#'   With `utf8_first = FALSE` the same rule refuses such bytes stated as
#'   windows-1252 or Latin-1 (see above).
#' @param argument The argument a refusal names for reading the file in
#'   another encoding (`"encoding"`, `"specification_encoding"`), or `NULL`
#'   when this file is read only as UTF-8 (then `encoding` is `"UTF-8"`).
#' @param ingest Whether a refusal also names the Ingest tab's control for
#'   it (Delimited file parsing and preview: the Encoding select, which
#'   applies to every CSV/TXT data file whether or not its explicit parsing
#'   choices are ticked, and the Delimiter). `FALSE` by default: only the
#'   Ingest tab's own reads (its Load and its preview) pass `TRUE`, so a
#'   console caller, such as [sieve_prepare_study()], is given the argument
#'   to pass and is not sent to a tab it does not have.
#' @return A list with `ok`; when `TRUE`, `bytes` (the file in UTF-8, byte
#'   order mark removed), `bom` (whether one was removed), `ascii` (whether
#'   every byte is ASCII) and `encoding` (the encoding the bytes were read
#'   in); when `FALSE`, `error` (the reason, a phrase that follows "could
#'   not be read: ").
#' @author Ken Kelley
#' @noRd
sieve_text_bytes <- function(path, encoding = "UTF-8", sep = NULL, check_separator = FALSE,
                             utf8_first = FALSE, argument = "encoding", ingest = FALSE) {
  refuse <- function(why) list(ok = FALSE, error = why)
  # Where the Ingest tab takes an encoding: every refusal that names one
  # names the select as well, since a select chosen for an earlier file
  # applies to the next one.
  on_ingest <- function(enc) {
    if (isTRUE(ingest)) sprintf(" (on the Ingest tab, Delimited file parsing and preview, Encoding %s)", enc) else ""
  }
  size <- file.size(path)
  if (is.na(size)) return(refuse("the file was not found"))
  bytes <- if (size > 0) readBin(path, "raw", size) else raw(0)
  starts <- function(...) length(bytes) >= length(c(...)) && identical(bytes[seq_along(c(...))], as.raw(c(...)))
  if (starts(0xFF, 0xFE) || starts(0xFE, 0xFF)) {
    return(refuse(paste(
      "it is saved as UTF-16 text (what Excel saves as Unicode Text), which SIEVER does not read;",
      "save it as CSV UTF-8 (Comma delimited), or as tab delimited text in UTF-8"
    )))
  }
  bom <- starts(0xEF, 0xBB, 0xBF)
  if (bom && isTRUE(utf8_first)) encoding <- "UTF-8"
  if (bom && !identical(encoding, "UTF-8")) {
    return(refuse(sprintf(
      "it begins with the UTF-8 byte order mark, so it is UTF-8, not %s; read it with %s = \"UTF-8\"%s",
      encoding, argument %||% "encoding", on_ingest("UTF-8")
    )))
  }
  if (bom) bytes <- bytes[-(1:3)]
  nul <- which(bytes == as.raw(0L))
  if (length(nul)) {
    return(refuse(sprintf(
      paste(
        "it holds a NUL byte (%s), which a text file does not hold; it may be a spreadsheet saved",
        "under a .csv or .txt name, or text in UTF-16. Save it as CSV UTF-8 (Comma delimited)"
      ),
      sieve_text_place(bytes, nul[1])
    )))
  }
  if (isTRUE(check_separator) && !is.null(sep)) {
    # The separators and the quotation mark are ASCII, so the header is
    # checked on its bytes in any encoding.
    wrong <- sieve_text_separator_problem(bytes, sep, ingest = ingest)
    if (!is.null(wrong)) return(refuse(wrong))
  }
  ascii <- !any(bytes > as.raw(0x7F))
  if (!ascii) {
    stated <- encoding
    bad <- if (identical(encoding, "UTF-8") || isTRUE(utf8_first)) sieve_utf8_first_invalid(bytes) else NA_integer_
    if (isTRUE(utf8_first) && is.na(bad)) encoding <- "UTF-8"
    byte_at <- function(at) {
      sprintf("%s: byte 0x%s", sieve_text_place(bytes, at, sep), toupper(as.character(bytes[at])))
    }
    if (identical(encoding, "UTF-8")) {
      if (!is.na(bad)) {
        return(refuse(if (is.null(argument)) {
          sprintf(
            paste(
              "it is not valid UTF-8 (%s), and this file is read only as UTF-8. Excel on Windows saves",
              "\"CSV (Comma delimited)\" in windows-1252: save the file as CSV UTF-8 (Comma delimited)",
              "and choose it again"
            ),
            byte_at(bad)
          )
        } else {
          sprintf(
            paste(
              "it is not valid UTF-8 (%s). Excel on Windows saves \"CSV (Comma delimited)\" in",
              "windows-1252: read it with %s = \"windows-1252\"%s, or save the file as CSV UTF-8"
            ),
            byte_at(bad), argument, on_ingest("windows-1252")
          )
        }))
      }
    } else {
      # Bytes that are valid UTF-8 and hold a character outside ASCII are
      # UTF-8 (see the rule above); decoded in the stated single byte
      # encoding, each such character would become two to four others with
      # no error. They are refused with the first such character as the
      # evidence, before the windows-1252 undefined bytes are looked for,
      # since those bytes are continuation bytes of UTF-8 as well (0x81 in
      # a capital A with an acute accent). Under utf8_first they were read
      # as UTF-8 above.
      if (!isTRUE(utf8_first) && is.na(sieve_utf8_first_invalid(bytes))) {
        at <- which(bytes > as.raw(0x7F))[1]
        lead <- as.integer(bytes[at])
        run <- bytes[at + seq_len(if (lead <= 0xDF) 2L else if (lead <= 0xEF) 3L else 4L) - 1L]
        char <- rawToChar(run)
        Encoding(char) <- "UTF-8"
        code <- utf8ToInt(char)
        single <- iconv(rawToChar(run), from = encoding, to = "UTF-8")
        # A character is quoted only when it prints (the C1 controls,
        # U+0080 to U+009F, do not).
        prints <- function(x) !is.na(x) && all(utf8ToInt(x) >= 0xA0)
        return(refuse(sprintf(
          paste(
            "it is valid UTF-8 and holds characters outside ASCII (%s: bytes %s, the UTF-8 form of %s), so it is",
            "UTF-8, not %s, which would read each such character as two to four others%s; read it with %s = \"UTF-8\"%s"
          ),
          sieve_text_place(bytes, at, sep), paste0("0x", toupper(as.character(run)), collapse = " "),
          if (prints(char)) sprintf("U+%04X \"%s\"", code, char) else sprintf("U+%04X", code),
          encoding,
          if (prints(single)) sprintf(" (this one as \"%s\")", single) else "",
          argument %||% "encoding", on_ingest("UTF-8")
        )))
      }
      neither <- if (isTRUE(utf8_first)) "neither UTF-8 nor valid" else "not valid"
      # windows-1252 leaves five bytes undefined; they are refused here, on
      # every platform, rather than by the converter, which on Windows maps
      # them to control characters.
      undefined <- if (identical(encoding, "windows-1252")) {
        which(bytes %in% as.raw(c(0x81, 0x8D, 0x8F, 0x90, 0x9D)))
      } else {
        integer(0)
      }
      text <- if (length(undefined)) NA_character_ else iconv(rawToChar(bytes), from = encoding, to = "UTF-8")
      if (is.na(text)) {
        return(refuse(sprintf(
          "it is %s %s%s; check the file's encoding%s",
          neither, encoding,
          if (length(undefined)) sprintf(" (%s)", byte_at(undefined[1])) else "",
          if (isTRUE(utf8_first)) "" else sprintf(" (UTF-8 files are read with %s = \"UTF-8\"%s)",
            argument %||% "encoding",
            if (isTRUE(ingest)) "; on the Ingest tab, Delimited file parsing and preview, Encoding UTF-8" else ""
          )
        )))
      }
      bytes <- charToRaw(enc2utf8(text))
    }
    if (!isTRUE(l10n_info()[["UTF-8"]])) {
      first <- which(bytes > as.raw(0x7F))[1]
      return(refuse(sprintf(
        paste(
          "it holds characters outside ASCII (%s), and this R session's character locale is %s, not",
          "UTF-8. SIEVER reads such a file only in a UTF-8 session, the one locale that represents every",
          "character (a C or POSIX locale represents none outside ASCII, a single byte locale such as",
          "Latin-1 only some), so that the file, and the session script that replays its read, give the",
          "same data frame on every machine. Start R in a UTF-8 locale (for example LANG=en_US.UTF-8 or",
          "C.UTF-8), or on Windows use R 4.2 or later, and read it again"
        ),
        sieve_text_place(bytes, first, sep), Sys.getlocale("LC_CTYPE")
      )))
    }
  }
  list(ok = TRUE, bytes = bytes, bom = bom, ascii = ascii, encoding = encoding)
}

#' The guard the session script writes before reading a file that holds
#' characters outside ASCII
#'
#' The script reads such a file only in a UTF-8 session, the rule
#' `sieve_text_bytes()` applies, so it gives the same data frame as the
#' reader or stops with the reason.
#'
#' @return R source lines.
#' @author Ken Kelley
#' @noRd
sieve_text_guard_code <- function() {
  c(
    "# The file holds characters outside ASCII, which SIEVER reads only in an R session whose locale",
    "# is UTF-8 (a C locale represents none of them, a Latin-1 locale only some); in another session",
    "# this stops rather than reading the file with columns renamed or rows dropped.",
    "stopifnot(isTRUE(l10n_info()[[\"UTF-8\"]]))"
  )
}

#' The separator a delimited file's header shows, when it is not the one
#' it is read with
#'
#' A file read with the wrong separator becomes one column whose name holds
#' every header field. The header line is checked before parsing: when the
#' separator it is to be read with does not occur in it and another
#' separator does, the file is refused with the separator it uses.
#'
#' @param bytes The file's bytes (the separators and the quotation mark are
#'   ASCII, so any ASCII-compatible encoding).
#' @param sep The separator it is to be read with.
#' @param ingest Whether the reason also names the Ingest tab's Delimiter
#'   (only the Ingest tab's own reads; see `sieve_text_bytes()`).
#' @return `NULL`, or the reason, a phrase.
#' @author Ken Kelley
#' @noRd
sieve_text_separator_problem <- function(bytes, sep, ingest = FALSE) {
  end <- which(bytes == as.raw(0x0A))[1]
  header <- as.integer(if (is.na(end)) bytes else bytes[seq_len(end - 1L)])
  outside <- cumsum(header == 0x22L) %% 2L == 0L
  count <- vapply(sieve_text_separators(), function(s) sum(header == utf8ToInt(s) & outside), numeric(1))
  if (count[[match(sep, sieve_text_separators())]] > 0 || !any(count > 0)) {
    return(NULL)
  }
  words <- names(sieve_text_separators())
  chosen <- words[match(sep, sieve_text_separators())]
  used <- names(which.max(count))
  k <- as.integer(max(count))
  sprintf(
    paste(
      "it was read with a %s as the separator, but its header holds no %s and %d %s%s, so it would read",
      "as one column: the file is separated by %ss. Read it with sep = \"%s\"%s%s"
    ),
    chosen, chosen, k, used, if (k == 1L) "" else "s", used,
    encodeString(sieve_text_separators()[[used]]),
    if (identical(used, "semicolon")) ", with dec = \",\" when its decimals use commas" else "",
    if (isTRUE(ingest)) {
      sprintf(
        " (on the Ingest tab, Delimited file parsing and preview, Use explicit CSV/TXT parsing choices, Delimiter %s)",
        paste0(toupper(substring(used, 1L, 1L)), substring(used, 2L))
      )
    } else {
      ""
    }
  )
}

#' Parse a delimited file that has passed `sieve_text_bytes()`
#'
#' The validated UTF-8 text is written to a temporary file that ends with a
#' line end and parsed there, so that its strings are in the session's
#' native encoding (as [utils::read.csv()] of the file gives them) and so
#' that a missing final line end, the one harmless cause of a parser
#' warning, cannot occur: a warning then means lost rows (an unmatched
#' quotation mark ends the parse at the end of the file, and the rows after
#' it are read as one field or not at all), and it is a refusal.
#'
#' @param checked The value of `sieve_text_bytes()`.
#' @param ... Arguments for [utils::read.table()] (not `file`).
#' @return The list `sieve_capture()` returns; `ok` is `FALSE` with the
#'   reason in `error` when the parser warned.
#' @author Ken Kelley
#' @noRd
sieve_text_parse <- function(checked, ...) {
  bytes <- checked$bytes
  if (length(bytes) && bytes[length(bytes)] != as.raw(0x0A)) bytes <- c(bytes, as.raw(0x0A))
  tmp <- tempfile("sieve_text_", fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(bytes, tmp)
  read <- sieve_capture(utils::read.table(tmp, ...))
  if (isTRUE(read$ok) && length(read$warnings)) {
    said <- gsub(tmp, "the file", read$warnings, fixed = TRUE)
    read$ok <- FALSE
    read$value <- NULL
    read$error <- sprintf(
      paste(
        "it could not be read completely (R said: %s), so it was not read at all rather than in part;",
        "a double quotation mark without its closing mark is the usual cause"
      ),
      paste(said, collapse = "; ")
    )
  }
  read$warnings <- character(0)
  read
}

#' The R that reads a delimited file, for the session script
#'
#' @param name The file name, written as an R string literal.
#' @param checked The value of `sieve_text_bytes()`.
#' @param encoding The stated encoding.
#' @param call The reading call: `"read.csv"`, `"read.delim"` (the defaults
#'   of each are left out), or `"read.table"`.
#' @param sep,dec The separator and the decimal mark.
#' @return A character vector of R source lines. The file encoding is
#'   written whenever the file holds a byte order mark or a character
#'   outside ASCII, and such a file is preceded by a check that the session
#'   is UTF-8, so the script reads the same data frame on every machine or
#'   says why it cannot.
#' @author Ken Kelley
#' @noRd
sieve_text_read_code <- function(name, checked, encoding, call = c("read.csv", "read.delim", "read.table"),
                                 sep = ",", dec = ".") {
  call <- match.arg(call)
  literal <- function(x) encodeString(x, quote = "\"")
  file_encoding <- if (identical(encoding, "UTF-8")) "UTF-8-BOM" else encoding
  plain <- isTRUE(checked$ascii) && !isTRUE(checked$bom)
  guard <- if (!isTRUE(checked$ascii)) sieve_text_guard_code()
  if (identical(call, "read.table")) {
    return(c(guard, sprintf(
      "d <- utils::read.table(%s, header = TRUE, sep = %s, dec = %s, quote = '\"', comment.char = '', fill = FALSE, fileEncoding = %s, stringsAsFactors = FALSE, check.names = TRUE)",
      literal(name), literal(sep), literal(dec), literal(file_encoding)
    )))
  }
  default_sep <- if (identical(call, "read.delim")) "\t" else ","
  args <- c(
    literal(name),
    if (!identical(sep, default_sep)) sprintf("sep = %s", literal(sep)),
    if (!identical(dec, ".")) sprintf("dec = %s", literal(dec)),
    if (!plain) sprintf("fileEncoding = %s", literal(file_encoding))
  )
  c(guard, sprintf("d <- %s(%s)", call, paste(args, collapse = ", ")))
}

#' Read delimited data with explicit, replayable parsing choices
#'
#' The Ingest tab's parsing override and its preview. The bytes are checked
#' first (`sieve_text_bytes()`), the header is checked for another
#' separator (`sieve_text_separator_problem()`), and the parse refuses
#' rather than returning part of the file (`sieve_text_parse()`). Since it
#' is the Ingest tab's own read, a refusal names the tab's controls as well
#' as the argument (`ingest = TRUE` in `sieve_text_bytes()`).
#'
#' @param path,name File path and display name.
#' @param sep Delimiter.
#' @param dec Decimal mark.
#' @param encoding File encoding, one of `sieve_text_encodings()` (`""`,
#'   the session's own, is read as UTF-8).
#' @param description Read SIEVER description rows.
#' @param nrows Number of rows, or -1 for the complete import.
#' @return Read outcome with the existing ingestion contract.
#' @author Ken Kelley
#' @noRd
sieve_read_delimited <- function(
  path,
  name,
  sep = ",",
  dec = ".",
  encoding = "UTF-8",
  description = TRUE,
  nrows = -1L
) {
  if (!sep %in% c(",", ";", "\t", "|")) stop("Choose comma, semicolon, tab, or vertical bar.", call. = FALSE)
  if (!dec %in% c(".", ",") || identical(sep, dec)) stop("Delimiter and decimal mark must differ.", call. = FALSE)
  allowed <- c("UTF-8", "latin1", "windows-1252", "")
  if (!encoding %in% allowed) stop("Choose a supported file encoding.", call. = FALSE)
  if (!nzchar(encoding)) encoding <- "UTF-8"
  failed <- function(why) {
    list(value = NULL, error = why, error_class = NULL, warnings = character(0), messages = character(0),
      ok = FALSE, description = NULL, measure = NULL, code = NULL)
  }
  # The Ingest tab's own read: a refusal names its controls as well.
  checked <- sieve_text_bytes(path, encoding, sep, check_separator = TRUE, ingest = TRUE)
  if (!isTRUE(checked$ok)) return(failed(checked$error))
  read <- sieve_text_parse(checked,
    header = TRUE,
    sep = sep,
    dec = dec,
    quote = "\"",
    comment.char = "",
    fill = FALSE,
    stringsAsFactors = FALSE, check.names = TRUE, nrows = nrows
  )
  read$description <- NULL
  read$measure <- NULL
  read$code <- sieve_text_read_code(name, checked, encoding, "read.table", sep = sep, dec = dec)
  if (isTRUE(read$ok) && description) {
    # The data rows are typed with the file's decimal mark.
    found <- sieve_description_rows(read$value, dec = dec)
    if (isTRUE(found$found)) {
      read$value <- found$data
      read$description <- found$description
      read$code <- c(
        read$code,
        sieve_description_code(dec = dec)
      )
    }
  }
  read
}

#' Validate reviewed one-to-one expert-item mappings
#' @param rated Rated item identifiers.
#' @param ledger Ledger item identifiers.
#' @param mapping Data frame with rated and ledger columns.
#' @return Checked mapping.
#' @author Ken Kelley
#' @noRd
sieve_check_content_mapping <- function(rated, ledger, mapping) {
  if (!is.data.frame(mapping) || !all(c(
    "rated",
    "ledger"
  ) %in% names(mapping))) stop("Supply rated and ledger identifiers.",
    call. = FALSE
  )
  if (anyNA(mapping$rated) || anyNA(mapping$ledger) || any(!mapping$rated %in% rated) || any(!mapping$ledger %in% ledger)) stop("Choose existing rated and ledger item identifiers.",
    call. = FALSE
  )
  if (anyDuplicated(mapping$rated) || anyDuplicated(mapping$ledger)) stop("Each rated item and each ledger item may appear only once. Disambiguate duplicate labels by variable ID.",
    call. = FALSE
  )
  mapping[, c("rated", "ledger"), drop = FALSE]
}

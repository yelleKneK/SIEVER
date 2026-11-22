# A comma or tab separated file reads the same way in every locale, or is
# refused with the reason. In a session whose character locale is C,
# sieve_read_data() must not rename zufrieden_a-umlaut to "zufrieden_..",
# keep an Excel byte order mark in the first name ("X...id"), or read a
# specification with one curly apostrophe as zero rows ("The specification
# has no rows."), and sieve_read_delimited() must not return 150 of 200
# rows with only a warning when row 150 holds a non-ASCII character. In a
# UTF-8 session a windows-1252 file must not be read with its accented
# letters as raw bytes, a semicolon file must not become one column, and an
# unmatched quotation mark must not return one row of four. The path route
# of sieve_prepare_study() takes a separator, a decimal mark and an
# encoding.
#
# Author: Ken Kelley

skip_if_not_installed("lavaan")

li_dir <- function() {
  dir <- tempfile("locale_")
  dir.create(dir)
  dir
}

# The file written byte for byte, so the test does not depend on the
# locale it runs in.
li_write <- function(dir, name, ...) {
  path <- file.path(dir, name)
  writeBin(do.call(c, lapply(list(...), function(x) if (is.raw(x)) x else charToRaw(enc2utf8(x)))), path)
  path
}

li_bom <- as.raw(c(0xEF, 0xBB, 0xBF))

# Runs code with the character locale given (C by default) and restores
# the locale after, whatever happens; skips where it cannot be set.
li_in_c <- function(code, locale = "C") {
  old <- Sys.getlocale("LC_CTYPE")
  set <- suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
  if (!nzchar(set)) skip(sprintf("the %s locale cannot be set in this session", locale))
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  force(code)
}

# The recorded R, evaluated beside the file, must give the same data frame.
li_replay <- function(read, path, name) {
  env <- new.env(parent = globalenv())
  code <- gsub(encodeString(name, quote = "\""), encodeString(path, quote = "\""), read$code, fixed = TRUE)
  eval(parse(text = code), env)
  env$d
}

test_that("non-ASCII column names read intact in a UTF-8 session and are refused, not renamed, in a C locale", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "the session is not UTF-8")
  dir <- li_dir()
  path <- li_write(dir, "umlaut.csv", "id,zufrieden_\u00e4,qualit\u00e9,gef\u00fchl_3\n1,2,3,4\n2,3,4,5\n3,4,5,6\n")
  read <- sieve_read_data(path, "umlaut.csv")
  expect_true(read$ok)
  expect_identical(names(read$value), c("id", "zufrieden_\u00e4", "qualit\u00e9", "gef\u00fchl_3"))
  # The script states the encoding and stops in a session that is not UTF-8.
  expect_true(any(grepl("stopifnot(isTRUE(l10n_info()[[\"UTF-8\"]]))", read$code, fixed = TRUE)))
  expect_true(any(grepl("d <- read.csv(\"umlaut.csv\", fileEncoding = \"UTF-8-BOM\")", read$code, fixed = TRUE)))
  expect_identical(li_replay(read, path, "umlaut.csv"), read$value)
  utf8 <- l10n_info()[["UTF-8"]]
  li_in_c({
    expect_false(isTRUE(l10n_info()[["UTF-8"]]))
    refused <- sieve_read_data(path, "umlaut.csv")
    expect_false(refused$ok)
    expect_null(refused$value)
    expect_match(refused$error, "holds characters outside ASCII (line 1 (the header), column 2)", fixed = TRUE)
    expect_match(refused$error, "this R session's character locale is C", fixed = TRUE)
    expect_match(refused$error, "Start R in a UTF-8 locale (for example LANG=en_US.UTF-8 or C.UTF-8)", fixed = TRUE)
    # The console read (sieve_frame_of(), behind sieve_prepare_study())
    # stops with the same reason.
    expect_error(SIEVER:::sieve_frame_of(path), "umlaut.csv could not be read: it holds characters outside ASCII",
      fixed = TRUE)
  })
  expect_identical(l10n_info()[["UTF-8"]], utf8)
})

test_that("a byte order mark is removed in every locale, and an ASCII file reads in a C locale", {
  dir <- li_dir()
  path <- li_write(dir, "excel.csv", li_bom, "id,x1,x2\n1,2,3\n4,5,6\n")
  read <- sieve_read_data(path, "excel.csv")
  expect_true(read$ok)
  expect_identical(names(read$value), c("id", "x1", "x2"))
  expect_identical(read$code, "d <- read.csv(\"excel.csv\", fileEncoding = \"UTF-8-BOM\")")
  expect_identical(li_replay(read, path, "excel.csv"), read$value)
  li_in_c({
    in_c <- sieve_read_data(path, "excel.csv")
    expect_true(in_c$ok)
    expect_identical(names(in_c$value), c("id", "x1", "x2"))
    expect_identical(in_c$value, read$value)
  })
  # A plain ASCII file keeps the plain recorded call.
  plain <- li_write(dir, "plain.csv", "id,x1\n1,2\n3,4\n")
  expect_identical(sieve_read_data(plain, "plain.csv")$code, "d <- read.csv(\"plain.csv\")")
})

test_that("a non-ASCII value in row 150 gives every row or a refusal, never 150 rows", {
  dir <- li_dir()
  lines <- c("id,town,x", sprintf("%d,%s,%d", 1:200, ifelse(1:200 == 150, "S\u00fcd", "Nord"), 1:200))
  path <- li_write(dir, "late.csv", paste0(paste(lines, collapse = "\n"), "\n"))
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    read <- sieve_read_data(path, "late.csv")
    expect_true(read$ok)
    expect_identical(nrow(read$value), 200L)
    expect_identical(read$value$town[150], "S\u00fcd")
    gui <- SIEVER:::sieve_read_delimited(path, "late.csv", ",", ".", "UTF-8", description = FALSE)
    expect_identical(nrow(gui$value), 200L)
  }
  li_in_c({
    for (read in list(
      sieve_read_data(path, "late.csv"),
      SIEVER:::sieve_read_delimited(path, "late.csv", ",", ".", "UTF-8", description = FALSE)
    )) {
      expect_false(read$ok)
      expect_null(read$value)
      expect_match(read$error, "holds characters outside ASCII (line 151, column town)", fixed = TRUE)
    }
  })
})

test_that("a specification whose wording holds one curly apostrophe is read, or refused with the reason, never as no rows", {
  dir <- li_dir()
  path <- li_write(dir, "study_specification.csv",
    "siever_specification,variable,role,wording\n2,x1,item,It\u2019s fine\n2,x2,item,plain\n")
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    spec <- SIEVER:::sieve_spec_read(path)
    expect_identical(nrow(spec), 2L)
    expect_identical(spec$wording, c("It\u2019s fine", "plain"))
  }
  li_in_c({
    err <- tryCatch(SIEVER:::sieve_spec_read(path), sieve_import_error = function(e) e)
    expect_s3_class(err, "sieve_import_error")
    expect_match(conditionMessage(err),
      "The specification could not be read: it holds characters outside ASCII (line 2, column wording)", fixed = TRUE)
    expect_false(grepl("no rows", conditionMessage(err), fixed = TRUE))
  })
})

test_that("a windows-1252 file is refused as not UTF-8 with the line, column and byte, and read with encoding = \"windows-1252\"", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "the session is not UTF-8")
  dir <- li_dir()
  path <- li_write(dir, "excel_windows.csv", "id,town,x\n1,Nord,2\n2,S", as.raw(0xFC), "d,3\n3,Ost,4\n")
  refused <- sieve_read_data(path, "excel_windows.csv")
  expect_false(refused$ok)
  expect_match(refused$error, "it is not valid UTF-8 (line 3, column town: byte 0xFC)", fixed = TRUE)
  expect_match(refused$error, "read it with encoding = \"windows-1252\"", fixed = TRUE)
  read <- sieve_read_data(path, "excel_windows.csv", encoding = "windows-1252")
  expect_true(read$ok)
  expect_identical(read$value$town, c("Nord", "S\u00fcd", "Ost"))
  expect_true(any(grepl("fileEncoding = \"windows-1252\"", read$code, fixed = TRUE)))
  expect_identical(li_replay(read, path, "excel_windows.csv"), read$value)
  gui <- SIEVER:::sieve_read_delimited(path, "excel_windows.csv", ",", ".", "windows-1252", description = FALSE)
  expect_identical(gui$value, read$value)
  # Each of the five bytes windows-1252 leaves undefined is named. They are
  # refused by SIEVER itself, not left to the converter: macOS libiconv
  # fails on them, but R for Windows converts through the Windows code page
  # tables, which map them to control characters, so the refusal would have
  # depended on the platform.
  for (byte in c(0x81, 0x8D, 0x8F, 0x90, 0x9D)) {
    bad <- li_write(dir, "undefined.csv", "id,town\n1,a", as.raw(byte), "\n")
    undefined <- sieve_read_data(bad, "undefined.csv", encoding = "windows-1252")
    expect_false(undefined$ok)
    expect_match(undefined$error,
      sprintf("it is not valid windows-1252 (line 2, column town: byte 0x%02X)", byte), fixed = TRUE)
  }
  # In Latin-1 the same bytes are control characters, which it defines.
  expect_true(sieve_read_data(bad, "undefined.csv", encoding = "latin1")$ok)
  # A UTF-8 file stated as windows-1252 is named by its byte order mark.
  marked <- li_write(dir, "marked.csv", li_bom, "id,x\n1,2\n")
  expect_match(sieve_read_data(marked, "marked.csv", encoding = "windows-1252")$error,
    "it begins with the UTF-8 byte order mark, so it is UTF-8, not windows-1252", fixed = TRUE)
})

test_that("on the Ingest tab the Encoding a refusal names reads the file with no other step, and every encoding refusal names it", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "the session is not UTF-8")
  # The Encoding select is read without Use explicit CSV/TXT parsing
  # choices ticked: a researcher who chooses windows-1252, as the refusal
  # says, and presses Load is not refused again, and neither is the
  # preview.
  dir <- li_dir()
  path <- li_write(dir, "survey.csv",
    "id,qualit", as.raw(0xE9), ",x1,x2\n1,3,4,2\n2,4,3,5\n3,2,5,4\n4,5,2,3\n5,3,3,4\n6,4,4,2\n")
  # Choosing the file does not end the session before any Load: the check
  # of its header for a specification must not stop in tolower() on the
  # byte 0xE9.
  expect_false(SIEVER:::sieve_spec_header(path))
  spec_1252 <- li_write(dir, "study.csv", "SIEVER_Specification,variable,qualit", as.raw(0xE9), "\n2,x1,a\n")
  expect_true(SIEVER:::sieve_spec_header(spec_1252))
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd",
      file = list(name = "survey.csv", datapath = path))
    session$setInputs(load = 1L)
    expect_null(shiny::isolate(state$meta))
    said <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", as.character(output$status_chip$html)))
    expect_match(said, "survey.csv was not loaded: it is not valid UTF-8 (line 1 (the header), column 2: byte 0xE9)",
      fixed = TRUE)
    expect_match(said, "(on the Ingest tab, Delimited file parsing and preview, Encoding windows-1252)", fixed = TRUE)
    expect_match(as.character(output$parse_preview$html), "Parsing preview refused", fixed = TRUE)
    # What the refusal says, and nothing else: the box stays unticked.
    session$setInputs(parse_encoding = "windows-1252")
    expect_false(isTRUE(input$parse_override))
    preview <- as.character(output$parse_preview$html)
    expect_false(grepl("refused", preview, fixed = TRUE))
    expect_match(preview, "4 columns; preview only, project unchanged.", fixed = TRUE)
    session$setInputs(load = 2L)
    expect_false(is.null(shiny::isolate(state$meta)))
    expect_true("qualit\u00e9" %in% names(shiny::isolate(state$items)))
    expect_equal(as.numeric(shiny::isolate(state$items)[["qualit\u00e9"]]), c(3, 4, 2, 5, 3, 4))
  })
  # The select comes before the box, which governs the delimiter and the
  # decimal mark below it.
  ui <- as.character(mod_ingest_ui("ingest"))
  expect_lt(regexpr("ingest-parse_encoding", ui, fixed = TRUE), regexpr("ingest-parse_override", ui, fixed = TRUE))
  expect_lt(regexpr("ingest-parse_override", ui, fixed = TRUE), regexpr("ingest-parse_sep", ui, fixed = TRUE))
  # A select left at windows-1252 for an earlier file applies to the next,
  # so the refusals a stated encoding gives on the Ingest tab's own read
  # (its Load passes ingest = TRUE) name the select as well.
  marked <- li_write(dir, "marked.csv", li_bom, "id,x\n1,2\n")
  expect_match(sieve_read_data(marked, "marked.csv", encoding = "windows-1252", ingest = TRUE)$error,
    "read it with encoding = \"UTF-8\" (on the Ingest tab, Delimited file parsing and preview, Encoding UTF-8)",
    fixed = TRUE)
  undefined <- li_write(dir, "undefined.csv", "id,town\n1,a", as.raw(0x81), "\n")
  expect_match(sieve_read_data(undefined, "undefined.csv", encoding = "windows-1252", ingest = TRUE)$error,
    paste(
      "check the file's encoding (UTF-8 files are read with encoding = \"UTF-8\";",
      "on the Ingest tab, Delimited file parsing and preview, Encoding UTF-8)"
    ),
    fixed = TRUE)
  # A file read outside the Ingest tab is not sent there.
  expect_false(grepl("Ingest tab",
    SIEVER:::sieve_text_bytes(marked, "windows-1252", ",", ingest = FALSE)$error, fixed = TRUE))
})

test_that("a console read is given the argument to pass and is not sent to the Ingest tab, whose own reads name its controls", {
  # sieve_prepare_study() and sieve_specification_template() (through
  # sieve_frame_of()) must not name the Ingest tab's Encoding select and
  # Delimiter in their refusals, although sieve_text_bytes() names them by
  # default. Only the Ingest tab's Load and preview name them.
  dir <- li_dir()
  bom <- li_write(dir, "marked.csv", li_bom, "id,x1,x2\n1,2,3\n2,3,4\n")
  wrong <- li_write(dir, "notutf8.csv", "id,town,x1\n1,S", as.raw(0xFC), "d,2\n2,Nord,3\n")
  undefined <- li_write(dir, "undefined.csv", "id,town,x1\n1,a", as.raw(0x81), ",2\n")
  semi <- li_write(dir, "european.csv", "id;x1;x2\n1;3,5;2\n2;1,5;4\n")
  console <- function(expr) tryCatch(expr, error = conditionMessage)
  said <- c(
    bom = console(SIEVER:::sieve_frame_of(bom, encoding = "windows-1252")),
    wrong = console(SIEVER:::sieve_frame_of(wrong)),
    undefined = console(SIEVER:::sieve_frame_of(undefined, encoding = "windows-1252")),
    semi = console(SIEVER:::sieve_frame_of(semi)),
    prepare = console(sieve_prepare_study(bom, data.frame(variable = "x1", role = "item"),
      file = file.path(dir, "p.siever.rds"), encoding = "windows-1252"
    )),
    template = console(sieve_specification_template(semi))
  )
  for (k in names(said)) {
    expect_false(grepl("Ingest tab", said[[k]], fixed = TRUE), info = k)
    expect_false(grepl("Delimited file parsing", said[[k]], fixed = TRUE), info = k)
  }
  # The console remedy, the argument to pass, is kept.
  expect_match(said[["bom"]], "so it is UTF-8, not windows-1252; read it with encoding = \"UTF-8\"", fixed = TRUE)
  expect_match(said[["wrong"]], "read it with encoding = \"windows-1252\", or save the file as CSV UTF-8", fixed = TRUE)
  expect_match(said[["undefined"]], "(UTF-8 files are read with encoding = \"UTF-8\")", fixed = TRUE)
  expect_match(said[["semi"]], "Read it with sep = \";\", with dec = \",\" when its decimals use commas", fixed = TRUE)
  expect_match(said[["prepare"]], "marked.csv could not be read: it begins with the UTF-8 byte order mark", fixed = TRUE)
  expect_match(said[["template"]], "the file is separated by semicolons", fixed = TRUE)
  # The Ingest tab's own reads name the control as well as the argument.
  expect_match(sieve_read_data(wrong, "notutf8.csv", ingest = TRUE)$error,
    "read it with encoding = \"windows-1252\" (on the Ingest tab, Delimited file parsing and preview, Encoding windows-1252)",
    fixed = TRUE)
  expect_match(sieve_read_data(semi, "european.csv", ingest = TRUE)$error,
    "(on the Ingest tab, Delimited file parsing and preview, Use explicit CSV/TXT parsing choices, Delimiter Semicolon)",
    fixed = TRUE)
  gui <- SIEVER:::sieve_read_delimited(semi, "european.csv", ",", ".", "UTF-8", description = FALSE)
  expect_match(gui$error, "Delimiter Semicolon)", fixed = TRUE)
  gui <- SIEVER:::sieve_read_delimited(bom, "marked.csv", ",", ".", "windows-1252", description = FALSE)
  expect_match(gui$error, "Encoding UTF-8)", fixed = TRUE)
})

test_that("a file whose bytes are valid UTF-8 is refused as UTF-8 under windows-1252 or latin1, never garbled, and a windows-1252 file still reads", {
  # With the Ingest tab's Encoding select applied without the explicit
  # parsing tick, a select left at windows-1252 for an earlier file must not
  # read the next file, valid UTF-8 with a header qualite with an acute e,
  # as "qualit\u00c3\u00a9" with no refusal or note, nor may the session
  # script replay such a misreading (read.csv(..., fileEncoding =
  # "windows-1252")). The same holds for sieve_prepare_study() and
  # sieve_specification_template() with encoding = "windows-1252".
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "the session is not UTF-8")
  dir <- li_dir()
  rows <- "\n1,3,4,2\n2,4,3,5\n3,2,5,4\n4,5,2,3\n5,3,3,4\n6,4,4,2\n"
  utf8 <- li_write(dir, "b.csv", "id,qualit\u00e9,x1,x2", rows)
  for (enc in c("windows-1252", "latin1")) {
    refused <- sieve_read_data(utf8, "b.csv", encoding = enc)
    expect_false(refused$ok)
    expect_null(refused$value)
    expect_match(refused$error, sprintf(paste(
      "it is valid UTF-8 and holds characters outside ASCII (line 1 (the header), column 2: bytes 0xC3 0xA9,",
      "the UTF-8 form of U+00E9 \"\u00e9\"), so it is UTF-8, not %s, which would read each such character as",
      "two to four others (this one as \"\u00c3\u00a9\"); read it with encoding = \"UTF-8\""
    ), enc), fixed = TRUE)
    expect_false(grepl("Ingest tab", refused$error, fixed = TRUE))
    gui <- SIEVER:::sieve_read_delimited(utf8, "b.csv", ",", ".", enc, description = FALSE)
    expect_false(gui$ok)
    expect_match(gui$error, sprintf("so it is UTF-8, not %s", enc), fixed = TRUE)
    expect_match(gui$error, "(on the Ingest tab, Delimited file parsing and preview, Encoding UTF-8)", fixed = TRUE)
  }
  read <- sieve_read_data(utf8, "b.csv")
  expect_true(read$ok)
  expect_identical(names(read$value), c("id", "qualit\u00e9", "x1", "x2"))
  # A continuation byte that windows-1252 leaves undefined (0x81 in a
  # capital A with an acute accent) is named as UTF-8, not as an undefined
  # byte, and a reading that would hold a control character is not quoted.
  capital <- li_write(dir, "capital.csv", "id,town\n1,\u00c1rvore\n")
  said <- sieve_read_data(capital, "capital.csv", encoding = "windows-1252")$error
  expect_match(said, "line 2, column town: bytes 0xC3 0x81, the UTF-8 form of U+00C1 \"\u00c1\"), so it is UTF-8, not windows-1252",
    fixed = TRUE)
  expect_false(grepl("not valid windows-1252", said, fixed = TRUE))
  expect_false(grepl("this one as", said, fixed = TRUE))
  # A genuine windows-1252 file, whose bytes are not valid UTF-8, still
  # reads as windows-1252, and its script says so.
  w1252 <- li_write(dir, "a.csv", "id,qualit", as.raw(0xE9), ",x1,x2", rows)
  old <- sieve_read_data(w1252, "a.csv", encoding = "windows-1252")
  expect_true(old$ok)
  expect_identical(names(old$value), c("id", "qualit\u00e9", "x1", "x2"))
  expect_true(any(grepl("fileEncoding = \"windows-1252\"", old$code, fixed = TRUE)))
  expect_true(sieve_read_data(w1252, "a.csv", encoding = "latin1")$ok)
  # The console functions follow the same rule, with their own arguments.
  expect_error(sieve_specification_template(utf8, encoding = "windows-1252"),
    "so it is UTF-8, not windows-1252, which would read each such character as two to four others", fixed = TRUE)
  expect_error(utils::capture.output(sieve_prepare_study(utf8, data.frame(variable = "x1", role = "item"),
    file = file.path(dir, "a.siever.rds"), encoding = "latin1"
  )), "b.csv could not be read: it is valid UTF-8", fixed = TRUE)
  expect_false(file.exists(file.path(dir, "a.siever.rds")))
  d <- data.frame(id = 1:6, x1 = c(3, 4, 2, 5, 3, 4), x2 = c(4, 3, 5, 2, 3, 4))
  template <- sieve_specification_template(d)
  template$construct[template$variable %in% c("x1", "x2")] <- "Scale"
  template$wording[template$variable == "x1"] <- "Qualit\u00e9"
  spec <- file.path(dir, "spec.csv")
  SIEVER:::sieve_spec_write(template, spec)
  err <- tryCatch(
    utils::capture.output(sieve_prepare_study(d, spec, file = file.path(dir, "s.siever.rds"),
      specification_encoding = "windows-1252"
    )),
    error = conditionMessage
  )
  expect_match(err, "column wording: bytes 0xC3 0xA9", fixed = TRUE)
  expect_match(err, "read it with specification_encoding = \"UTF-8\"", fixed = TRUE)
  expect_false(grepl("Ingest tab", err, fixed = TRUE))
  # Unstated, the specification is read as UTF-8 (utf8_first).
  utils::capture.output(study <- sieve_prepare_study(d, spec, file = file.path(dir, "t.siever.rds"),
    encoding = "windows-1252"
  ))
  expect_identical(study$meta$wording[study$meta$item == "x1"], "Qualit\u00e9")
  # On the Ingest tab: a.csv loads in windows-1252; the select left there,
  # b.csv (UTF-8) is refused in the preview and by the Load, which leaves
  # a.csv in the session; with the select at UTF-8, it loads as written.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd", parse_encoding = "windows-1252",
      file = list(name = "a.csv", datapath = w1252))
    session$setInputs(load = 1L)
    expect_identical(shiny::isolate(state$dataset_name), "a.csv")
    expect_true("qualit\u00e9" %in% names(shiny::isolate(state$items)))
    session$setInputs(file = list(name = "b.csv", datapath = utf8))
    preview <- as.character(output$parse_preview$html)
    expect_match(preview, "Parsing preview refused:", fixed = TRUE)
    expect_match(preview, "so it is UTF-8, not windows-1252", fixed = TRUE)
    session$setInputs(load = 2L)
    session$setInputs(load_replace = 1L)
    expect_identical(shiny::isolate(state$dataset_name), "a.csv")
    said <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", as.character(output$status_chip$html)))
    expect_match(said, "b.csv was not loaded: it is valid UTF-8 and holds characters outside ASCII", fixed = TRUE)
    expect_match(said, "read it with encoding = \"UTF-8\" (on the Ingest tab, Delimited file parsing and preview, Encoding UTF-8)",
      fixed = TRUE)
    expect_false(any(grepl("windows-1252", shiny::isolate(state$codelog), fixed = TRUE) &
      grepl("b.csv", shiny::isolate(state$codelog), fixed = TRUE)))
    session$setInputs(parse_encoding = "UTF-8")
    expect_match(as.character(output$parse_preview$html), "4 columns; preview only, project unchanged.", fixed = TRUE)
    session$setInputs(load = 3L)
    session$setInputs(load_replace = 2L)
    expect_identical(shiny::isolate(state$dataset_name), "b.csv")
    expect_true("qualit\u00e9" %in% names(shiny::isolate(state$items)))
    expect_equal(as.numeric(shiny::isolate(state$items)[["qualit\u00e9"]]), c(3, 4, 2, 5, 3, 4))
  })
  # The Ingest tab's help and the user guide say so.
  expect_match(paste(as.character(mod_ingest_ui("ingest")), collapse = " "),
    "and under windows-1252 or latin1 it is refused with UTF-8 named", fixed = TRUE)
  guide_file <- testthat::test_path("..", "..", "vignettes", "user-guide.Rmd")
  if (!file.exists(guide_file)) guide_file <- system.file("doc", "user-guide.Rmd", package = "SIEVER")
  skip_if(!nzchar(guide_file) || !file.exists(guide_file), "the user guide is neither beside the tests nor installed")
  guide <- gsub("\\s+", " ", paste(readLines(guide_file, warn = FALSE), collapse = " "))
  expect_match(guide, "is refused under windows-1252 or `latin1`, with UTF-8 to choose", fixed = TRUE)
})

test_that("the UTF-8 check visits only the bytes outside ASCII and finds the same first invalid byte", {
  # A walk of every byte, as the reference: about 0.3 seconds a megabyte
  # byte compiled (2 seconds on the first call) when the first byte outside
  # ASCII is near the end, which every windows-1252 read would pay, since
  # the check runs on it.
  walk <- function(bytes) {
    b <- as.integer(bytes)
    n <- length(b)
    i <- 1L
    while (i <= n) {
      x <- b[i]
      len <- if (x < 0x80) 1L else if (x >= 0xC2 && x <= 0xDF) 2L else if (x >= 0xE0 && x <= 0xEF) 3L else
        if (x >= 0xF0 && x <= 0xF4) 4L else 0L
      if (!len || i + len - 1L > n) return(i)
      if (len > 1L) {
        cont <- b[i + seq_len(len - 1L)]
        if (any(cont < 0x80 | cont > 0xBF)) return(i)
        if ((x == 0xE0 && cont[1] < 0xA0) || (x == 0xED && cont[1] > 0x9F) ||
            (x == 0xF0 && cont[1] < 0x90) || (x == 0xF4 && cont[1] > 0x8F)) return(i)
      }
      i <- i + len
    }
    NA_integer_
  }
  check <- SIEVER:::sieve_utf8_first_invalid
  cases <- list(
    raw(0), charToRaw("id,x\n1,2\n"), charToRaw(enc2utf8("qualit\u00e9 \u2019 \U0001F600")),
    as.raw(c(0x61, 0xC3)), as.raw(c(0xC3, 0x41)), as.raw(c(0xE0, 0x80, 0x80)), as.raw(c(0xED, 0xA0, 0x80)),
    as.raw(c(0xF0, 0x8F, 0x80, 0x80)), as.raw(c(0xF4, 0x90, 0x80, 0x80)), as.raw(c(0xC0, 0x80)),
    as.raw(c(0x41, 0xC3, 0xA9, 0xFC)), as.raw(c(0xC3, 0xA9, 0xC3, 0xA9, 0x81))
  )
  for (x in cases) expect_identical(check(x), walk(x), info = paste(as.character(x), collapse = " "))
  set.seed(1)
  for (r in 1:300) {
    x <- as.raw(sample(c(0:127, 128:255, rep(c(0xC3, 0xE2, 0xF0, 0x80, 0xA9, 0xBF), 20)), sample(1:30, 1), replace = TRUE))
    expect_identical(check(x), walk(x))
  }
  big <- c(charToRaw(strrep("1,2,3,4\n", 625000)), as.raw(0xE9))
  expect_identical(check(big), length(big))
  # A timing is not asked of CRAN's machines, nor of a busy one.
  skip_on_cran()
  skip_unless_timing()
  check(big)
  expect_lt(system.time(check(big))[["elapsed"]], 0.5)
})

test_that("a semicolon file is refused with its separator named, and read with sep and dec", {
  dir <- li_dir()
  path <- li_write(dir, "european.csv", "id;x1;x2\n1;3,5;2\n2;1,5;4\n3;2,25;6\n")
  refused <- sieve_read_data(path, "european.csv")
  expect_false(refused$ok)
  expect_match(refused$error, "its header holds no comma and 2 semicolons", fixed = TRUE)
  expect_match(refused$error, "Read it with sep = \";\", with dec = \",\" when its decimals use commas", fixed = TRUE)
  gui <- SIEVER:::sieve_read_delimited(path, "european.csv", ",", ".", "UTF-8", description = FALSE)
  expect_false(gui$ok)
  expect_match(gui$error, "the file is separated by semicolons", fixed = TRUE)
  read <- sieve_read_data(path, "european.csv", sep = ";", dec = ",")
  expect_true(read$ok)
  expect_identical(read$value$x1, c(3.5, 1.5, 2.25))
  expect_identical(read$code, "d <- read.csv(\"european.csv\", sep = \";\", dec = \",\")")
  expect_identical(li_replay(read, path, "european.csv"), read$value)
  # A comma separated .txt file, which is read as tab separated by default.
  txt <- li_write(dir, "commas.txt", "id,x1\n1,2\n")
  expect_match(sieve_read_data(txt, "commas.txt")$error, "its header holds no tab and 1 comma", fixed = TRUE)
  expect_error(sieve_read_data(path, "european.csv", sep = ",", dec = ","), "must differ", fixed = TRUE)
  expect_error(sieve_read_data(path, "european.csv", sep = ":"), "`sep` must be one of", fixed = TRUE)
  expect_error(sieve_read_data(path, "european.csv", encoding = "UTF-16"), "`encoding` must be one of", fixed = TRUE)
})

test_that("a parse that would return part of the file is refused, not truncated", {
  dir <- li_dir()
  path <- li_write(dir, "quote.csv", "id,town,x\n1,a,2\n2,\"b,3\n3,c,4\n4,d,5\n")
  # read.csv() returns one row (row 4) with only a warning about an
  # incomplete final line.
  read <- sieve_read_data(path, "quote.csv")
  expect_false(read$ok)
  expect_null(read$value)
  expect_match(read$error, "it could not be read completely", fixed = TRUE)
  expect_match(read$error, "a double quotation mark without its closing mark is the usual cause", fixed = TRUE)
  gui <- SIEVER:::sieve_read_delimited(path, "quote.csv", ",", ".", "UTF-8", description = FALSE)
  expect_false(gui$ok)
  # A file without a final line end is not a lost row.
  open <- li_write(dir, "open.csv", "id,x\n1,2\n3,4")
  expect_identical(sieve_read_data(open, "open.csv")$value, data.frame(id = c(1L, 3L), x = c(2L, 4L)))
})

test_that("UTF-16 text and a file with NUL bytes are refused as not readable text", {
  dir <- li_dir()
  utf16 <- li_write(dir, "unicode.txt", as.raw(c(0xFF, 0xFE, 0x69, 0x00, 0x64, 0x00)))
  expect_match(sieve_read_data(utf16, "unicode.txt")$error, "it is saved as UTF-16 text", fixed = TRUE)
  nul <- li_write(dir, "binary.csv", "id,x\n1,", as.raw(0L), "\n")
  expect_match(sieve_read_data(nul, "binary.csv")$error, "it holds a NUL byte (line 2)", fixed = TRUE)
})

test_that("sieve_prepare_study() and sieve_specification_template() read a semicolon, comma-decimal, windows-1252 file by their arguments", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "the session is not UTF-8")
  dir <- li_dir()
  x1 <- c(3.5, 1.5, 2.25, 4, 2.75, 3.25, 1.75, 4.5, 2, 3)
  x2 <- c(2, 4.5, 3.25, 1.5, 3.75, 2.5, 4.25, 1.25, 3, 2.75)
  x3 <- c(1.5, 2.5, 3.5, 4.5, 2, 3, 4, 1, 2.25, 3.75)
  comma <- function(v) sub(".", ",", format(v), fixed = TRUE)
  # Excel on Windows in a locale whose decimal mark is a comma: semicolons,
  # decimal commas, windows-1252 (0xFC is u with an umlaut), CRLF.
  rows <- lapply(1:10, function(i) {
    c(
      charToRaw(sprintf("%d;", i)),
      if (i %% 2L == 1L) c(charToRaw("S"), as.raw(0xFC), charToRaw("d")) else charToRaw("Nord"),
      charToRaw(sprintf(";%s;%s;%s\r\n", comma(x1)[i], comma(x2)[i], comma(x3)[i]))
    )
  })
  path <- file.path(dir, "survey.csv")
  writeBin(c(charToRaw("id;region;x1;x2;x3\r\n"), do.call(c, rows)), path)
  # The defaults name what is wrong.
  expect_error(sieve_specification_template(path), "the file is separated by semicolons", fixed = TRUE)
  expect_error(sieve_specification_template(path, sep = ";", dec = ","),
    "it is not valid UTF-8 (line 2, column region: byte 0xFC)", fixed = TRUE)
  template <- sieve_specification_template(path, sep = ";", dec = ",", encoding = "windows-1252")
  expect_identical(template$variable, c("id", "region", "x1", "x2", "x3"))
  template$construct[template$variable %in% c("x1", "x2", "x3")] <- "Scale"
  template$role[template$variable %in% c("x1", "x2", "x3")] <- "item"
  template$type[template$variable %in% c("x1", "x2", "x3")] <- "continuous"
  template$status[template$variable %in% c("x1", "x2", "x3")] <- "included"
  # A specification that a spreadsheet saved with semicolons, read by the
  # separator its header shows.
  spec_path <- file.path(dir, "survey_specification.csv")
  utils::write.table(template, spec_path, sep = ";", row.names = FALSE, na = "", fileEncoding = "UTF-8")
  project <- file.path(dir, "survey.siever.rds")
  utils::capture.output(study <- sieve_prepare_study(path, spec_path,
    file = project, sep = ";", dec = ",",
    encoding = "windows-1252"
  ))
  expect_true(file.exists(project))
  expect_identical(study$raw$x1, x1)
  expect_identical(study$raw$region, rep(c("S\u00fcd", "Nord"), 5))
  expect_match(paste(study$codelog, collapse = "\n"),
    "d <- read.csv(\"survey.csv\", sep = \";\", dec = \",\", fileEncoding = \"windows-1252\")", fixed = TRUE)
  expect_error(sieve_prepare_study(path, spec_path, file = project, sep = ",", dec = ","), "must differ", fixed = TRUE)
})

test_that("a session that is not UTF-8 is refused by the rule SIEVER applies, not by a claim that the locale cannot represent the file", {
  dir <- li_dir()
  # qualite with e acute, in Latin-1, which a Latin-1 locale can represent.
  path <- li_write(dir, "latin.csv", "id,qualit", as.raw(0xE9), "\n1,2\n2,3\n")
  li_in_c(locale = "en_US.ISO8859-1", {
    expect_false(isTRUE(l10n_info()[["UTF-8"]]))
    refused <- sieve_read_data(path, "latin.csv", encoding = "latin1")
    expect_false(refused$ok)
    # The refusal does not say the Latin-1 locale "cannot represent them",
    # which is false for this file.
    expect_false(grepl("cannot represent", refused$error, fixed = TRUE))
    expect_match(refused$error, "this R session's character locale is en_US.ISO8859-1, not UTF-8", fixed = TRUE)
    expect_match(refused$error, "SIEVER reads such a file only in a UTF-8 session", fixed = TRUE)
    expect_match(refused$error, "a single byte locale such as Latin-1 only some", fixed = TRUE)
    expect_match(refused$error, "or on Windows use R 4.2 or later", fixed = TRUE)
  })
  # The session script states the same rule, in the data and the content
  # ratings scripts alike (one helper writes both guards).
  guard <- SIEVER:::sieve_text_guard_code()
  expect_false(any(grepl("cannot represent", guard, fixed = TRUE)))
  expect_match(paste(guard, collapse = " "), "which SIEVER reads only in an R session whose locale # is UTF-8", fixed = TRUE)
  expect_identical(guard[length(guard)], "stopifnot(isTRUE(l10n_info()[[\"UTF-8\"]]))")
  content <- deparse(body(SIEVER:::mod_content_server))
  expect_false(any(grepl("cannot represent", content, fixed = TRUE)))
})

test_that("sieve_prepare_study() reads a UTF-8 specification beside a windows-1252 data file as written, and a windows-1252 one by its own argument", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "the session is not UTF-8")
  dir <- li_dir()
  x1 <- c(3.5, 1.5, 2.25, 4, 2.75, 3.25, 1.75, 4.5, 2, 3)
  x2 <- c(2, 4.5, 3.25, 1.5, 3.75, 2.5, 4.25, 1.25, 3, 2.75)
  x3 <- c(1.5, 2.5, 3.5, 4.5, 2, 3, 4, 1, 2.25, 3.75)
  # Data that Excel on Windows saved as CSV (Comma delimited): windows-1252,
  # 0xFC is u with an umlaut.
  rows <- lapply(1:10, function(i) {
    c(
      charToRaw(sprintf("%d,", i)),
      if (i %% 2L == 1L) c(charToRaw("S"), as.raw(0xFC), charToRaw("d")) else charToRaw("Nord"),
      charToRaw(sprintf(",%s,%s,%s\n", x1[i], x2[i], x3[i]))
    )
  })
  data_1252 <- file.path(dir, "survey.csv")
  writeBin(c(charToRaw("id,region,x1,x2,x3\n"), do.call(c, rows)), data_1252)
  data_utf8 <- file.path(dir, "survey_utf8.csv")
  utils::write.csv(data.frame(id = 1:10, region = rep(c("S\u00fcd", "Nord"), 5), x1 = x1, x2 = x2, x3 = x3),
    data_utf8, row.names = FALSE, fileEncoding = "UTF-8")
  # SIEVER's template, filled in with wordings that hold a curly apostrophe
  # and an accented letter, and written as SIEVER writes it (UTF-8).
  template <- sieve_specification_template(data_1252, encoding = "windows-1252")
  items <- template$variable %in% c("x1", "x2", "x3")
  template$construct[items] <- "Scale"
  template$role[items] <- "item"
  template$type[items] <- "continuous"
  template$status[items] <- "included"
  template$wording[match(c("x1", "x2"), template$variable)] <- c("It\u2019s fine", "Qualit\u00e9")
  spec_utf8 <- file.path(dir, "survey_specification.csv")
  SIEVER:::sieve_spec_write(template, spec_utf8)
  wording_of <- function(study) study$meta$wording[match(c("x1", "x2"), study$meta$item)]
  # The specification is not read in the data file's encoding, which would
  # give "It\u00e2\u20ac\u2122s fine" and "Qualit\u00c3\u00a9", written into
  # the project with no warning.
  utils::capture.output(study <- sieve_prepare_study(data_1252, spec_utf8,
    file = file.path(dir, "a.siever.rds"), encoding = "windows-1252"
  ))
  expect_identical(wording_of(study), c("It\u2019s fine", "Qualit\u00e9"))
  expect_identical(study$raw$region, rep(c("S\u00fcd", "Nord"), 5))
  # The specification Excel on Windows saved (windows-1252) beside its data
  # file is read in the data file's encoding.
  spec_1252 <- file.path(dir, "excel_specification.csv")
  utils::write.csv(template, spec_1252, row.names = FALSE, na = "", fileEncoding = "windows-1252")
  expect_false(validUTF8(rawToChar(readBin(spec_1252, "raw", file.size(spec_1252)))))
  utils::capture.output(study_1252 <- sieve_prepare_study(data_1252, spec_1252,
    file = file.path(dir, "b.siever.rds"), encoding = "windows-1252"
  ))
  expect_identical(wording_of(study_1252), c("It\u2019s fine", "Qualit\u00e9"))
  # Beside a UTF-8 data file it is refused with its own argument named, and
  # read with it.
  err <- tryCatch(
    utils::capture.output(sieve_prepare_study(data_utf8, spec_1252, file = file.path(dir, "c.siever.rds"))),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "it is not valid UTF-8 (line 4, column wording: byte 0x92)", fixed = TRUE)
  expect_match(conditionMessage(err),
    "read it with specification_encoding = \"windows-1252\", or save the file as CSV UTF-8", fixed = TRUE)
  expect_false(grepl("Ingest tab", conditionMessage(err), fixed = TRUE))
  expect_false(file.exists(file.path(dir, "c.siever.rds")))
  utils::capture.output(study_stated <- sieve_prepare_study(data_utf8, spec_1252,
    file = file.path(dir, "d.siever.rds"), specification_encoding = "windows-1252"
  ))
  expect_identical(wording_of(study_stated), c("It\u2019s fine", "Qualit\u00e9"))
  # A stated encoding is taken at its word: a UTF-8 file with its byte
  # order mark stated as windows-1252 is refused by that mark.
  spec_bom <- file.path(dir, "bom_specification.csv")
  writeBin(c(li_bom, readBin(spec_utf8, "raw", file.size(spec_utf8))), spec_bom)
  expect_error(
    utils::capture.output(sieve_prepare_study(data_utf8, spec_bom,
      file = file.path(dir, "e.siever.rds"), specification_encoding = "windows-1252"
    )),
    "so it is UTF-8, not windows-1252; read it with specification_encoding = \"UTF-8\"", fixed = TRUE
  )
  expect_error(
    sieve_prepare_study(data_utf8, spec_utf8, file = file.path(dir, "f.siever.rds"), specification_encoding = "UTF-16"),
    "`specification_encoding` must be NULL", fixed = TRUE
  )
})

test_that("a specification on the Ingest tab and a content ratings file, read only as UTF-8, are told to be saved as CSV UTF-8", {
  dir <- li_dir()
  spec <- li_write(dir, "study_specification.csv",
    "siever_specification,variable,role,wording\n2,x1,item,Qualit", as.raw(0xE9), "\n")
  ratings <- li_write(dir, "ratings.csv", "item,Jos", as.raw(0xE9), ",Ann\nx1,4,3\nx2,3,4\n")
  spec_err <- tryCatch(SIEVER:::sieve_spec_read(spec), sieve_import_error = function(e) conditionMessage(e))
  rated <- SIEVER:::sieve_parse_ratings(ratings, "ratings.csv")
  expect_false(rated$ok)
  for (said in c(spec_err, rated$message)) {
    expect_match(said, "it is not valid UTF-8", fixed = TRUE)
    expect_match(said, "this file is read only as UTF-8", fixed = TRUE)
    expect_match(said, "save the file as CSV UTF-8 (Comma delimited) and choose it again", fixed = TRUE)
    # Neither points to the Ingest tab's encoding choice, which reads data
    # files only.
    expect_false(grepl("encoding = \"windows-1252\"", said, fixed = TRUE))
    expect_false(grepl("Ingest tab", said, fixed = TRUE))
  }
  expect_match(spec_err, "line 2, column wording: byte 0xE9", fixed = TRUE)
  expect_match(rated$message, "line 1 (the header), column 2: byte 0xE9", fixed = TRUE)
})

test_that("the content ratings import reads its bytes the same way", {
  dir <- li_dir()
  path <- li_write(dir, "ratings.csv", li_bom, "item,expert_1,expert_2\nx1,4,3\nx2,3,4\nx3,4,4\n")
  res <- SIEVER:::sieve_parse_ratings(path, "ratings.csv")
  expect_true(res$ok)
  expect_identical(rownames(res$ratings), c("x1", "x2", "x3"))
  expect_true(res$text$bom)
  wide <- li_write(dir, "accent.csv", "item,expert_1,expert_2\nqualit\u00e9,4,3\nx2,3,4\nx3,4,4\n")
  li_in_c({
    refused <- SIEVER:::sieve_parse_ratings(wide, "accent.csv")
    expect_false(refused$ok)
    expect_match(refused$message, "accent.csv could not be read: it holds characters outside ASCII (line 2, column item)",
      fixed = TRUE)
  })
})

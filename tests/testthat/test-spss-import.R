# Item types from an SPSS file: five or more response options are
# continuous by default, with a clear way to change it, and a note warns
# the user when SPSS says an item is ordered. haven does not return the
# SPSS measurement level, so SIEVER reads it from the .sav dictionary
# (fct_spss.R) and the Ingest tab names the items SPSS calls ordinal or
# nominal that are typed continuous, with a button that sets them ordered;
# a "Set the type of" control sets every included item, or a construct's or
# a facet's items, at once.

skip_if_not_installed("haven")
skip_if_not_installed("shiny")

ns_strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = " ")))

# Set the measurement level of every variable of a .sav file written by
# haven (1 Nominal, 2 Ordinal, 3 Scale), in place, by rewriting the measure
# of each entry of the variable display record (record 7, subtype 11).
ns_set_measures <- function(path, measures) {
  con <- file(path, open = "r+b")
  on.exit(close(con))
  ri <- function(n = 1L) readBin(con, "integer", n, size = 4, endian = "little")
  skip <- function(n) if (n > 0) invisible(readBin(con, "raw", n))
  skip(4 + 60 + 4 + 16 + 8 + 9 + 8 + 64 + 3)
  n_vars <- 0L
  at <- NA_real_
  per <- NA_integer_
  repeat {
    rt <- ri()
    if (rt == 2L) {
      type <- ri()
      has_label <- ri()
      n_missing <- ri()
      ri(2L)
      skip(8)
      if (type != -1L) n_vars <- n_vars + 1L
      if (has_label == 1L) skip(4 * ceiling(ri() / 4))
      if (n_missing != 0L) skip(8 * abs(n_missing))
    } else if (rt == 3L) {
      for (i in seq_len(ri())) {
        skip(8)
        len <- as.integer(readBin(
          con,
          "raw",
          1
        ))
        skip(8 * ceiling((len + 1) / 8) - 1)
      }
      ri()
      skip(4 * ri())
    } else if (rt == 6L) {
      skip(80 * ri())
    } else if (rt == 7L) {
      subtype <- ri()
      size <- ri()
      count <- ri()
      if (subtype == 11L) {
        at <- seek(con, rw = "read")
        per <- count %/% n_vars
      }
      skip(size * count)
    } else if (rt == 999L) {
      break
    }
  }
  stopifnot(is.finite(at), length(measures) == n_vars)
  for (i in seq_len(n_vars)) {
    seek(con, at + 4 * per * (i - 1L), rw = "write")
    writeBin(as.integer(measures[i]), con, size = 4, endian = "little")
  }
  invisible(path)
}

# A small study: an identifier and six seven-point items (two constructs).
ns_sav <- function(measures) {
  set.seed(7)
  z <- stats::rnorm(150)
  d <- data.frame(pid = seq_len(150))
  for (j in 1:6) {
    d[[sprintf("q%d", j)]] <- as.numeric(cut(z + stats::rnorm(150), c(-Inf, -1.5, -0.8, -0.25, 0.25, 0.8, 1.5, Inf),
      labels = FALSE
    ))
  }
  path <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "study.sav")
  haven::write_sav(d, path)
  ns_set_measures(path, measures)
  path
}

test_that("the SPSS measurement level of every variable is read from the .sav dictionary", {
  path <- ns_sav(c(3L, 2L, 2L, 1L, 3L, 2L, 2L))
  dict <- sieve_spss_dictionary(path)
  expect_identical(dict$name, c("pid", sprintf("q%d", 1:6)))
  expect_identical(dict$measure_label, c("Scale", "Ordinal", "Ordinal", "Nominal", "Scale", "Ordinal", "Ordinal"))
  m <- sieve_spss_measures(path)
  expect_identical(unname(m[c("q1", "q3", "q4")]), c("Ordinal", "Nominal", "Scale"))
  # The names agree with the columns haven reads.
  expect_identical(names(m), names(haven::read_sav(path)))
  # Something that is not a system file gives no levels, and no error.
  bad <- tempfile(fileext = ".sav")
  writeLines("not spss", bad)
  expect_null(sieve_spss_measures(bad))
})

test_that("a .sav read keeps the levels on the columns and the ledger names the ordinal items typed continuous", {
  path <- ns_sav(c(3L, 2L, 2L, 1L, 3L, 2L, 2L))
  read <- sieve_read_data(path, "study.sav")
  expect_true(read$ok)
  expect_identical(attr(read$value$q1, "spss_measure"), "Ordinal")
  st <- sieve_new_state()
  shiny::isolate(sieve_load_frame(st, read$value, "study.sav", read$code))
  meta <- shiny::isolate(st$meta)
  expect_identical(meta$spss_measure[match(c("q1", "q3", "q4"), meta$item)], c("Ordinal", "Nominal", "Scale"))
  # Seven response options: continuous, so the ordinal and nominal ones clash.
  expect_true(all(meta$type == "continuous"))
  clash <- sieve_spss_level_conflicts(meta)
  expect_identical(clash$item, c("q1", "q2", "q3", "q5", "q6"))
  # A .csv carries no level: no column, no clash.
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(haven::zap_labels(haven::read_sav(path)), csv, row.names = FALSE)
  st2 <- sieve_new_state()
  r2 <- sieve_read_data(csv, "study.csv")
  shiny::isolate(sieve_load_frame(st2, r2$value, "study.csv", r2$code))
  expect_false("spss_measure" %in% names(shiny::isolate(st2$meta)))
  expect_identical(nrow(sieve_spss_level_conflicts(shiny::isolate(st2$meta))), 0L)
})

test_that("an SPSS file whose items are Scale gives no note", {
  path <- ns_sav(rep(3L, 7))
  read <- sieve_read_data(path, "study.sav")
  st <- sieve_new_state()
  shiny::isolate(sieve_load_frame(st, read$value, "study.sav", read$code))
  expect_identical(nrow(sieve_spss_level_conflicts(shiny::isolate(st$meta))), 0L)
})

test_that("the Ingest tab warns about the items SPSS calls ordinal, sets them ordered in one click, and sets any construct's items at once", {
  path <- ns_sav(c(3L, 2L, 2L, 1L, 3L, 2L, 2L))
  read <- sieve_read_data(path, "study.sav")
  st <- sieve_new_state()
  shiny::isolate(sieve_load_frame(st, read$value, "study.sav", read$code))
  m <- shiny::isolate(st$meta)
  m$construct <- rep(c("A", "B"), each = 3)
  st$meta <- m
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$flushReact()
    note <- ns_strip(output$spss_note$html)
    expect_match(note, "SPSS calls 5 of the items typed continuous here ordinal or nominal (4 Ordinal and 1 Nominal)",
      fixed = TRUE
    )
    expect_match(note, "q1, q2, q3, q5, q6", fixed = TRUE)
    expect_match(note, "Set these 5 items to ordered", fixed = TRUE)
    session$setInputs(spss_to_ordered = 1L)
    meta <- shiny::isolate(state$meta)
    expect_identical(meta$type[match(c("q1", "q2", "q3", "q5", "q6"), meta$item)], rep("ordered", 5))
    expect_identical(meta$type[match("q4", meta$item)], "continuous")
    expect_false(isTRUE(shiny::isolate(state$confirmed)))
    expect_null(output$spss_note)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, 'meta$type[meta$item %in% c("q1", "q2", "q3", "q5", "q6")] <- "ordered"', fixed = TRUE)
    # A construct's items at once, back to continuous.
    session$setInputs(types_scope = "construct:A", types_to = "continuous")
    session$setInputs(types_apply = 1L)
    meta <- shiny::isolate(state$meta)
    expect_identical(meta$type[match(c("q1", "q2", "q3"), meta$item)], rep("continuous", 3))
    expect_identical(meta$type[match(c("q5", "q6"), meta$item)], rep("ordered", 2))
    expect_match(ns_strip(output$spss_note$html),
      "SPSS calls 3 of the items typed continuous here ordinal or nominal (2 Ordinal and 1 Nominal)",
      fixed = TRUE
    )
    # Every included item to ordered.
    session$setInputs(types_scope = "all", types_to = "ordered")
    session$setInputs(types_apply = 2L)
    expect_true(all(shiny::isolate(state$meta)$type == "ordered"))
    expect_null(output$spss_note)
  })
})

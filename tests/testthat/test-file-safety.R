# Files SIEVER reads hold only plain data: a project file (the container,
# and the plain RDS form a saved study may have), a session input file,
# and a frozen norm reference are checked type by
# type, without dispatching a method or forcing a promise, before any field
# is read. Each crafted file below runs code (it writes a marker file) if a
# reader dispatches a method on it or forces a promise in it; each must be
# refused with the place and the type or class, and the marker must never
# appear. Genuine files open.

# ---- Crafting files that run code when touched ----------------------------------

fs_marker <- function() {
  path <- tempfile("file_safety_marker_")
  unlink(path)
  path
}

fs_int <- function(i) writeBin(as.integer(i), raw(), endian = "big")

# The bytes of one serialized item with its stream header removed: "X\n",
# three integers, the length of the native encoding's name, then the name.
fs_item_bytes <- function(object) {
  s <- serialize(object, NULL, xdr = TRUE, version = 3L)
  nelen <- readBin(s[15:18], "integer", size = 4L, endian = "big")
  s[-seq_len(18L + nelen)]
}

# An unforced promise (PROMSXP with the global environment as its
# environment, no value yet, and code that writes the marker), as it is
# written in a serialization stream. No R function returns such an object,
# so it is written by hand, as an attacker would.
fs_promise_bytes <- function(marker, value = "SIEVER project") {
  code <- bquote({
    writeLines("ran", .(marker))
    .(value)
  })
  c(fs_int(0x405), fs_int(253L), fs_int(252L), fs_item_bytes(code))
}

# Serialize `object`, in which the integer 123456789L stands where the
# promise goes (an element or an attribute), and put the promise there. The
# promise's code adds its own symbols to the reader's reference table, so a
# back reference written after it to a symbol first written after it would
# point elsewhere; an object whose later parts use symbols (names, class,
# row.names) more than once defines them first (see `primer` below).
fs_with_promise <- function(object, marker, value = "SIEVER project") {
  s <- serialize(object, NULL, xdr = TRUE, version = 3L)
  sentinel <- c(fs_int(13L), fs_int(1L), fs_int(123456789L))
  at <- which(vapply(seq_len(length(s) - 11L), function(i) identical(s[i:(i + 11L)], sentinel), logical(1)))
  stopifnot(length(at) == 1L)
  c(s[seq_len(at - 1L)], fs_promise_bytes(marker, value), s[(at + 12L):length(s)])
}

# A function, with the global environment as its own, that writes the marker
# and returns `value`.
fs_payload <- function(marker, value) {
  f <- eval(bquote(function(...) {
    writeLines("ran", .(marker))
    .(value)
  }))
  environment(f) <- globalenv()
  f
}

# An environment whose `field` is an active binding that writes the marker.
fs_active_env <- function(marker, field = "format", value = "SIEVER project", class = NULL) {
  e <- new.env(parent = emptyenv())
  makeActiveBinding(field, fs_payload(marker, value), e)
  if (!is.null(class)) class(e) <- class
  e
}

# A class of the test's own whose `$` and names() methods write the marker,
# registered for the test and removed after it.
fs_register_probe_class <- function(marker, env = parent.frame()) {
  table <- asNamespace("base")[[".__S3MethodsTable__."]]
  registerS3method("$", "file_safety_probe", function(x, name) {
    writeLines("ran", marker)
    .subset2(x, name)
  }, envir = baseenv())
  registerS3method("names", "file_safety_probe", function(x) {
    writeLines("ran", marker)
    attr(x, "names", exact = TRUE)
  }, envir = baseenv())
  withr::defer(rm(list = c("$.file_safety_probe", "names.file_safety_probe"), envir = table), envir = env)
}

# The file forms: a project container written with a valid header and digest
# (the digest proves nothing about who wrote a file), and a plain gzip RDS.
fs_container <- function(bytes, schema = sieve_project_schema()) {
  payload <- memCompress(bytes, type = "gzip")
  header <- paste(c(
    "SIEVER project container 1", sprintf("schema %d", schema),
    sprintf("payload gzip %d", length(payload)), sprintf("digest md5 %s", sieve_bytes_digest(payload)), "", ""
  ), collapse = "\n")
  path <- tempfile("crafted_", fileext = ".siever.rds")
  writeBin(c(charToRaw(header), payload), path)
  path
}

fs_rds <- function(bytes, ext = ".rds") {
  path <- tempfile("crafted_", fileext = ext)
  con <- gzfile(path, "wb")
  writeBin(bytes, con)
  close(con)
  path
}

fs_bytes <- function(object) serialize(object, NULL, xdr = TRUE, version = 3L)

fs_reference <- function(cluster = FALSE) {
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- data.frame(
    item = names(items), label = names(items),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
  reference <- if (cluster) {
    sieve_norm_freeze(items, meta,
      name = "Clustered calibration", population = "Grade 7 and 8 students, 1939",
      design = list(unit = "clustered", cluster = "school"), cluster = lavaan::HolzingerSwineford1939$school
    )
  } else {
    sieve_norm_freeze(items, meta, name = "Calibration", population = "Grade 7 and 8 students, 1939")
  }
  list(reference = reference, items = items, meta = meta)
}

# ---- The guard itself ----------------------------------------------------------

test_that("the plain-data guard refuses every type that is not data and every class that is not a class of data, by place", {
  marker <- fs_marker()
  probe <- function(x) sieve_plain_unserialized(list(x), "x")
  expect_identical(probe(fs_active_env(marker)), "x: type environment")
  expect_identical(probe(function() 1), "x: type closure")
  expect_identical(probe(list(a = 1, f = sum)), "x$f: type builtin")
  expect_identical(probe(list(q = quote(a + b))), "x$q: type language")
  expect_identical(probe(list(as.name("a"))), "x[[1]]: type symbol")
  expect_identical(probe(list(p = new("externalptr"))), "x$p: type externalptr")
  expect_identical(probe(list(s = asS4(list(1)))), "x$s: an S4 object")
  expect_identical(probe(list(f = y ~ x)), "x$f: type language")
  expect_identical(probe(structure(list(1), class = "reactivevalues")), "x: class reactivevalues")
  expect_identical(probe(list(d = data.frame(a = 1:2), g = structure(list(), class = c("R6", "list")))), "x$g: class R6")
  expect_identical(
    probe(list(a = structure(1:3, hook = function() 1))),
    "attr(x$a, \"hook\"): type closure"
  )
  # A promise anywhere: in a list element and in an attribute.
  held <- unserialize(fs_with_promise(list(format = "a", slot = 123456789L), marker))
  expect_identical(sieve_plain_unserialized(list(held), "x"), "x$slot: type promise")
  held <- unserialize(fs_with_promise(list(format = "a", v = structure(1:2, hook = 123456789L)), marker))
  expect_identical(sieve_plain_unserialized(list(held), "x"), "attr(x$v, \"hook\"): type promise")
  expect_false(file.exists(marker))
  # The classes of data, and the result classes of SIEVER, DMAR and lavaan, pass.
  plain <- list(
    d = data.frame(a = 1:3, f = factor(c("a", "b", "a")), o = factor(1:3, ordered = TRUE)),
    when = as.Date("2020-01-01") + 0:2, stamp = as.POSIXct("2020-01-01", tz = "UTC"),
    span = as.difftime(3, units = "mins"),
    labelled = haven::labelled_spss(c(1, 2, 9), labels = c(low = 1, refused = 9), na_values = 9, label = "q"),
    fit = structure(c(cfi = 0.95), class = c("lavaan.vector", "numeric")),
    reliability = structure(list(estimate = 0.8), class = c("dmar_reliability", "dmar_tbl", "data.frame")),
    snapshot = structure(list(format = "SIEVER project"), class = "sieve_project"),
    nothing = NULL, bytes = as.raw(1:3), z = 1i, nested = list(list(list(TRUE)))
  )
  expect_identical(probe(plain), character(0))
  expect_identical(probe(NULL), character(0))
})

# ---- Project files ---------------------------------------------------------------

test_that("a project file that holds an environment, a promise, a closure or a foreign class is refused before any field is read, and nothing in it runs", {
  marker <- fs_marker()
  fs_register_probe_class(marker)
  schema <- sieve_project_schema()
  base <- list(format = "SIEVER project", schema_version = schema)
  # An environment classed as a data frame in raw, whose active binding
  # would run if a reader such as sieve_labelled_ready() read its column
  # classes.
  raw_env <- c(base, list(raw = fs_active_env(marker, "x1", 1:3, class = c("environment", "data.frame"))))
  # A promise in schema_version, the field the container check reads
  # first.
  promise_schema <- fs_with_promise(list(format = "SIEVER project", schema_version = 123456789L), marker, schema)
  # A promise in format, and one in an attribute of a column.
  promise_format <- fs_with_promise(list(format = 123456789L, schema_version = schema), marker)
  primer <- list(primer = data.frame())
  promise_attr <- fs_with_promise(
    c(primer, base, list(raw = data.frame(a = structure(1:2, hook = 123456789L)))), marker
  )
  # A classed object whose `$` method runs if a reader dispatches on it
  # (snapshot$format).
  dollar <- structure(base, class = "file_safety_probe")
  # A classed object inside, whose names() method a dispatching walk calls.
  named <- c(base, list(extra = structure(list(a = 1), class = "file_safety_probe")))
  # A closure, at the top and deep inside.
  hook <- c(base, list(analyses = list(A1 = list(hook = fs_payload(marker, "run")))))
  cases <- list(
    list(bytes = fs_bytes(raw_env), what = "snapshot$raw: type environment"),
    list(bytes = promise_schema, what = "snapshot$schema_version: type promise"),
    list(bytes = promise_format, what = "snapshot$format: type promise"),
    list(bytes = promise_attr, what = "attr(snapshot$raw$a, \"hook\"): type promise"),
    list(bytes = fs_bytes(dollar), what = "snapshot: class file_safety_probe"),
    list(bytes = fs_bytes(named), what = "snapshot$extra: class file_safety_probe"),
    list(bytes = fs_bytes(hook), what = "snapshot$analyses$A1$hook: type closure"),
    list(bytes = fs_bytes(fs_payload(marker, base)), what = "snapshot: type closure"),
    list(bytes = fs_bytes(fs_active_env(marker)), what = "snapshot: type environment"),
    list(bytes = fs_bytes(c(base, list(raw = asS4(list(1))))), what = "snapshot$raw: an S4 object")
  )
  expect_false(file.exists(marker))
  for (case in cases) {
    for (form in c("container", "legacy")) {
      path <- if (identical(form, "container")) fs_container(case$bytes) else fs_rds(case$bytes, ".siever.rds")
      err <- expect_error(sieve_project_read(path), class = "sieve_project_error", info = paste(form, case$what))
      expect_match(conditionMessage(err),
        sprintf("The file holds something that is not plain data (%s), so it was not opened.", case$what),
        fixed = TRUE, info = paste(form, case$what)
      )
      expect_match(conditionMessage(err), "Project files hold only data; nothing in the file was run.",
        fixed = TRUE, info = paste(form, case$what)
      )
      # In a reactive context, as the app's Open project dialog reads a file.
      expect_error(shiny::isolate(sieve_project_read(path)), "not plain data", class = "sieve_project_error")
      expect_false(file.exists(marker), info = paste(form, case$what))
    }
  }
})

test_that("a genuine project opens from the container and from the plain RDS form, with labelled, date and time columns", {
  skip_if_not_installed("haven")
  n <- 40L
  labels <- c(Low = 1, High = 5, Refused = 9)
  item <- function(v) haven::labelled_spss(as.numeric(v), labels = labels, na_values = 9, label = "an item")
  d <- withr::with_seed(1, data.frame(
    id = seq_len(n),
    when = as.Date("2020-01-01") + seq_len(n),
    stamp = as.POSIXct("2020-01-01 10:00:00", tz = "UTC") + 3600 * seq_len(n),
    q1 = item(sample(1:5, n, TRUE)), q2 = item(sample(1:5, n, TRUE)),
    q3 = item(sample(1:5, n, TRUE)), q4 = item(sample(c(1:5, 9), n, TRUE))
  ))
  # A time of day, as haven reads an SPSS TIME column (class hms).
  d$clock <- structure(60 * seq_len(n), units = "secs", class = c("hms", "difftime"))
  sav <- tempfile(fileext = ".sav")
  haven::write_sav(d, sav)
  read <- sieve_read_data(sav, name = "dates.sav")
  st <- sieve_new_state()
  expect_true(sieve_load_frame(st, read$value, "dates.sav", read$code)$ok)
  shiny::isolate(sieve_confirm_types(st, "button"))
  snap <- sieve_project_snapshot(st)
  expect_true(all(c("Date", "POSIXct", "hms") %in% unlist(lapply(snap$raw, class))))
  container <- tempfile(fileext = ".siever.rds")
  sieve_project_write(snap, container)
  legacy <- tempfile(fileext = ".siever.rds")
  saveRDS(unclass(snap), legacy)
  for (path in c(container, legacy)) {
    back <- sieve_project_read(path)
    expect_identical(back$raw, snap$raw)
    expect_identical(back$items, snap$items)
    expect_identical(back$meta, snap$meta)
  }
  expect_identical(attr(sieve_project_read(container), "container"), 1L)
  expect_identical(attr(sieve_project_read(legacy), "container"), 0L)
})

# ---- Session input files -----------------------------------------------------------

test_that("a session input file that holds an environment, a promise or a foreign class is refused before any field is read, and nothing in it runs", {
  marker <- fs_marker()
  fs_register_probe_class(marker)
  base <- list(format = "SIEVER replay inputs", version = 1L)
  cases <- list(
    list(bytes = fs_bytes(structure(base, class = "file_safety_probe")), what = "store: class file_safety_probe"),
    list(bytes = fs_bytes(fs_active_env(marker, value = "SIEVER replay inputs")), what = "store: type environment"),
    list(
      bytes = fs_with_promise(list(format = 123456789L, version = 1L), marker, "SIEVER replay inputs"),
      what = "store$format: type promise"
    ),
    list(bytes = fs_bytes(c(base, list(revisions = list(k = fs_payload(marker, 1))))), what = "store$revisions$k: type closure")
  )
  for (case in cases) {
    for (compressed in c(TRUE, FALSE)) {
      bytes <- if (compressed) memCompress(case$bytes, type = "gzip") else case$bytes
      path <- tempfile(fileext = ".rds")
      writeBin(bytes, path)
      err <- expect_error(sieve_replay_read(path, sieve_bytes_digest(bytes)), info = case$what)
      expect_match(conditionMessage(err),
        sprintf("The session input file holds something that is not plain data (%s), so it was not read.", case$what),
        fixed = TRUE, info = case$what
      )
      expect_match(conditionMessage(err), "nothing in the file was run", fixed = TRUE)
      expect_false(file.exists(marker), info = case$what)
    }
  }
})

test_that("a genuine session input file is read back whole", {
  d <- DMAR::holzinger_swineford[, c("id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges")]
  spec <- sieve_specification_template(d)
  spec$construct[spec$role == "item"] <- "Visual"
  path <- tempfile("holzinger_", fileext = ".siever.rds")
  invisible(utils::capture.output(sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")))
  paths <- sieve_session_write(sieve_project_read(path), tempfile("siever_session_"))
  script <- readLines(paths[["script"]])
  keys <- regmatches(script, regexpr("[0-9a-f]{32}", script))
  store <- sieve_replay_read(paths[["inputs"]], keys[1])
  expect_identical(store$format, "SIEVER replay inputs")
  bound <- new.env()
  sieve_replay_bind(store, keys[2], envir = bound)
  expect_identical(dim(bound$items), c(301L, 3L))
})

# ---- Frozen norm references -----------------------------------------------------------

test_that("a frozen reference file that holds an environment, a promise, a closure or a foreign class is refused by sieve_norm_read(), and nothing in it runs", {
  marker <- fs_marker()
  fs_register_probe_class(marker)
  fx <- fs_reference()
  good <- fx$reference
  # The genuine reference with its definition replaced by an environment of
  # active bindings.
  nested <- good
  nested$definition <- fs_active_env(marker, "Visual", good$definition$Visual)
  closure <- good
  closure$definition$Visual$weights <- fs_payload(marker, c(1, 1, 1))
  dollar <- good
  class(dollar) <- c("file_safety_probe", "siever_norm_reference")
  cases <- list(
    list(
      bytes = fs_bytes(fs_active_env(marker, value = "SIEVER frozen norm reference 1", class = "siever_norm_reference")),
      what = "reference: type environment"
    ),
    list(bytes = fs_bytes(nested), what = "reference$definition: type environment"),
    list(bytes = fs_bytes(closure), what = "reference$definition$Visual$weights: type closure"),
    list(bytes = fs_bytes(dollar), what = "reference: class file_safety_probe"),
    list(
      bytes = fs_with_promise(
        structure(list(format = 123456789L, name = "Promise"), class = "siever_norm_reference"),
        marker, "SIEVER frozen norm reference 1"
      ),
      what = "reference$format: type promise"
    )
  )
  expect_false(file.exists(marker))
  for (case in cases) {
    path <- fs_rds(case$bytes)
    err <- expect_error(sieve_norm_read(path), info = case$what)
    expect_match(conditionMessage(err),
      sprintf("The file holds something that is not plain data (%s), so it was not imported.", case$what),
      fixed = TRUE, info = case$what
    )
    expect_match(conditionMessage(err), "A frozen norm reference holds only data; nothing in the file was run.",
      fixed = TRUE
    )
    expect_false(file.exists(marker), info = case$what)
  }
  # The same objects handed to the functions that use a reference.
  for (object in list(
    fs_active_env(marker, value = "SIEVER frozen norm reference 1", class = "siever_norm_reference"), nested, closure, dollar
  )) {
    expect_error(sieve_norm_apply(object, fx$items, fx$meta), "reference holds something that is not plain data")
    expect_error(sieve_norm_transform_limits(object, B = 99L, seed = 1), "reference holds something that is not plain data")
    expect_false(file.exists(marker))
  }
})

test_that("a frozen reference that is plain data but malformed is refused with the field named", {
  fx <- fs_reference()
  good <- fx$reference
  refused <- function(object, pattern) {
    path <- tempfile(fileext = ".rds")
    saveRDS(object, path)
    expect_error(sieve_norm_read(path), pattern, fixed = TRUE)
    expect_error(sieve_norm_apply(object, fx$items, fx$meta), pattern, fixed = TRUE)
  }
  refused(datasets::mtcars, "it is not of class siever_norm_reference")
  x <- good
  x$format <- "SIEVER frozen norm reference 2"
  refused(x, "reference$format is not \"SIEVER frozen norm reference 1\"")
  x <- good
  x$table$sd <- NULL
  refused(x, "reference$table is not a data frame with a row per construct and the columns construct, mean and sd")
  x <- good
  x$scores$Speed <- NULL
  refused(x, "reference$scores is not a list with the calibration scores named for each construct")
  x <- good
  x$scores$Visual <- c(x$scores$Visual, NA)
  refused(x, "reference$scores$Visual is not a nonempty vector of finite scores")
  x <- good
  x$definition$Textual$weights <- c(1, 1)
  refused(x, "reference$definition$Textual$weights is not a number for each of its 3 items")
  x <- good
  x$name <- c("a", "b")
  refused(x, "reference$name is not a single nonempty character string")
  x <- good
  x$design <- list(unit = 1)
  refused(x, "reference$design is neither NULL nor a list whose unit is a single character string")
  path <- tempfile(fileext = ".rds")
  writeLines("not an R data file", path)
  expect_error(sieve_norm_read(path), "could not be read as an R data file", fixed = TRUE)
  expect_error(sieve_norm_read(tempfile()), "was not found", fixed = TRUE)
})

test_that("genuine frozen references, independent and clustered, are read back unchanged and apply as the originals do", {
  for (clustered in c(FALSE, TRUE)) {
    fx <- fs_reference(cluster = clustered)
    path <- tempfile(fileext = ".rds")
    saveRDS(fx$reference, path)
    back <- sieve_norm_read(path)
    expect_identical(back, fx$reference)
    expect_identical(sieve_norm_apply(back, fx$items, fx$meta), sieve_norm_apply(fx$reference, fx$items, fx$meta))
    expect_identical(is.null(back$bootstrap_scores), !clustered)
    note <- sieve_norm_reference_contents_text(back)
    expect_match(note, "writes respondent-level data", fixed = TRUE)
    expect_identical(grepl("row number and cluster label", note, fixed = TRUE), clustered)
  }
})

test_that("the Scoring and Norms page imports through sieve_norm_read(): a crafted file is refused with the reason and runs nothing, a genuine one is kept", {
  marker <- fs_marker()
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, configure = function(s) {
    s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
    s$estimator <- "MLR"
  })
  genuine <- shiny::isolate(sieve_norm_freeze(s$items, s$meta, s$scoring_policies,
    name = "Imported calibration", population = "Grade 7 and 8 students, 1939"
  ))
  good_path <- tempfile(fileext = ".rds")
  saveRDS(genuine, good_path)
  crafted <- fs_rds(fs_bytes(fs_active_env(marker, value = "SIEVER frozen norm reference 1", class = "siever_norm_reference")))
  nested <- genuine
  nested$definition <- fs_active_env(marker, "Visual", genuine$definition$Visual)
  nested_path <- fs_rds(fs_bytes(nested))
  shiny::testServer(mod_finalize_server, args = list(state = s), {
    for (path in c(crafted, nested_path)) {
      session$setInputs(norm_import = list(name = "shared.rds", datapath = path))
      expect_match(norm_feedback(), "The file holds something that is not plain data (reference", fixed = TRUE)
      expect_match(norm_feedback(), "nothing in the file was run", fixed = TRUE)
      expect_length(shiny::isolate(state$norm_references), 0L)
      expect_false(file.exists(marker))
    }
    session$setInputs(norm_import = list(name = "calibration.rds", datapath = good_path))
    expect_identical(norm_feedback(), "Imported reference: Imported calibration")
    expect_identical(shiny::isolate(state$norm_references)[["Imported calibration"]], genuine)
    session$setInputs(norm_reference = "Imported calibration")
    note <- paste(as.character(output$norm_reference_note$html), collapse = " ")
    expect_match(note, "Download frozen reference (.rds) writes respondent-level data", fixed = TRUE)
  })
  expect_false(file.exists(marker))
})

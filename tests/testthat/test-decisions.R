# Decision records: the researcher's choice with its rationale (optional: a
# record made without one says "no rationale given"), the inputs in force,
# and an append-only history in which a changed decision is a new record
# that names the one it replaces.

decision_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = NULL)
  s
}

decisions_of <- function(state) shiny::isolate(state$decisions)

test_that("a new state holds an empty decision list", {
  s <- sieve_new_state()
  expect_identical(decisions_of(s), list())
  tab <- shiny::isolate(sieve_decisions_table(s))
  expect_s3_class(tab, "data.frame")
  expect_identical(nrow(tab), 0L)
})

test_that("a decision without a rationale is recorded as 'no rationale given', and the other fields are still required", {
  # Written evidence for the number of factors is not required, so an
  # absent or blank rationale is recorded as the words "no rationale
  # given"; a rationale that is not text at all is refused.
  s <- decision_state()
  log_before <- shiny::isolate(s$codelog)
  rec <- function(...) sieve_record_decision(s, "factor_retention", "How many factors?", 3L, ...)
  expect_error(rec(rationale = 3), "rationale")
  expect_error(rec(rationale = c("one", "two")), "rationale")
  expect_identical(decisions_of(s), list())
  expect_identical(shiny::isolate(s$codelog), log_before)
  for (none in list(
    quote(rec()), quote(rec(rationale = NULL)), quote(rec(rationale = "")),
    quote(rec(rationale = "   \n ")), quote(rec(rationale = NA_character_))
  )) {
    r <- eval(none)
    expect_identical(r$rationale, sieve_no_rationale())
    expect_identical(r$rationale, "no rationale given")
    expect_identical(r$choice, 3L)
  }
  recs <- decisions_of(s)
  expect_length(recs, 5L)
  expect_identical(recs$D5$supersedes, "D4")
  log <- shiny::isolate(s$codelog)
  expect_identical(sum(grepl("# Rationale:  no rationale given", log, fixed = TRUE)), 5L)
  tab <- shiny::isolate(sieve_decisions_table(s))
  expect_identical(unique(tab$rationale), "no rationale given")

  # The other fields are checked too: a record must say what was decided.
  expect_error(sieve_record_decision(s, "", "Q?", 3L, "because"), "topic")
  expect_error(sieve_record_decision(s, "t", " ", 3L, "because"), "question")
  expect_error(sieve_record_decision(s, "t", "Q?", NA, "because"), "choice")
  expect_error(sieve_record_decision(s, "t", "Q?", c(2, 3), "because"), "choice")
  expect_error(sieve_record_decision(s, "t", "Q?", "", "because"), "choice")
  expect_length(decisions_of(s), 5L)
})

test_that("a record keeps the choice, the rationale, the evidence, the inputs, the version, and the time", {
  s <- decision_state()
  rev_now <- sieve_revision(s)
  before <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), tz = "UTC")
  rec <- sieve_record_decision(
    s,
    topic = "factor_retention",
    question = "How many factors are retained for the 9 included items?",
    choice = 3L, rationale = "  Three match the theorized structure.  ",
    evidence = c("parallel analysis: 3", NA, "", "MAP: 2")
  )
  after <- Sys.time()

  expect_identical(rec$id, "D1")
  expect_identical(rec$topic, "factor_retention")
  expect_identical(rec$choice, 3L) # stored as given, not as text
  expect_identical(rec$rationale, "Three match the theorized structure.")
  expect_identical(rec$evidence, c("parallel analysis: 3", "MAP: 2"))
  expect_true(is.na(rec$supersedes))
  # The inputs in force are the ones the shared contract reports.
  expect_identical(rec$revision, rev_now)
  expect_identical(rec$revision$dataset_id, "D1")
  expect_match(rec$revision$data_key, "^[0-9a-f]{32}$")
  expect_match(rec$revision$meta_key, "^[0-9a-f]{32}$")
  expect_identical(rec$version, sieve_version())
  # An ISO 8601 UTC time between the moments before and after the call.
  expect_match(rec$time, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
  stamp <- as.POSIXct(rec$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_true(stamp >= before && stamp <= after + 1)

  # It is what the state now holds, under its id, as a plain list.
  held <- decisions_of(s)
  expect_named(held, "D1")
  expect_identical(held$D1, rec)
  expect_true(is.list(rec) && !is.object(rec))
  # Evidence is optional.
  rec2 <- sieve_record_decision(s, "model_choice", "Which model?", "three factor", "Best interpretability.")
  expect_identical(rec2$id, "D2")
  expect_identical(rec2$evidence, character(0))
  expect_identical(rec2$choice, "three factor")
})

test_that("a changed decision is a new record that names the one it replaces, and nothing is edited in place", {
  s <- decision_state()
  d1 <- sieve_record_decision(s, "factor_retention", "How many factors?", 3L, "Theory.")
  other <- sieve_record_decision(s, "model_choice", "Which model?", "A", "Fit and meaning.")
  d3 <- sieve_record_decision(s, "factor_retention", "How many factors?", 2L, "The third factor did not replicate.")
  held <- decisions_of(s)

  expect_identical(names(held), c("D1", "D2", "D3"))
  expect_identical(d3$supersedes, "D1")
  expect_true(is.na(other$supersedes)) # a different topic replaces nothing
  # The earlier records are exactly as they were written.
  expect_identical(held$D1, d1)
  expect_identical(held$D2, other)
  expect_identical(held$D1$choice, 3L)

  # A third revision replaces the second, not the first.
  d4 <- sieve_record_decision(s, "factor_retention", "How many factors?", 3L, "New data favored three.")
  expect_identical(d4$supersedes, "D3")
  expect_identical(sieve_current_decision(decisions_of(s), "factor_retention")$id, "D4")
  expect_identical(sieve_current_decision(decisions_of(s), "model_choice")$id, "D2")
  expect_null(sieve_current_decision(decisions_of(s), "no_such_topic"))

  # A decision belongs to the data it was made on: after another dataset is
  # loaded, a decision on the same topic starts a new chain, and the record
  # carries the new inputs while the old records keep theirs.
  old_rev <- d4$revision
  s$items <- shiny::isolate(s$items)[1:150, , drop = FALSE]
  s$raw <- shiny::isolate(s$raw)[1:150, , drop = FALSE]
  shiny::isolate(sieve_new_dataset(s))
  d5 <- sieve_record_decision(s, "factor_retention", "How many factors?", 1L, "A different sample.")
  expect_true(is.na(d5$supersedes))
  expect_identical(d5$revision$dataset_id, "D2")
  expect_false(identical(d5$revision$data_key, old_rev$data_key))
  expect_identical(decisions_of(s)$D4$revision, old_rev)
  expect_identical(sieve_current_decision(decisions_of(s), "factor_retention", "D1")$id, "D4")
  expect_identical(sieve_current_decision(decisions_of(s), "factor_retention", "D2")$id, "D5")
})

test_that("the decision table has one row per record and derives what is current", {
  s <- decision_state()
  sieve_record_decision(s, "factor_retention", "How many factors?", 3L, "Theory.",
    evidence = c("parallel analysis: 3", "MAP: 2")
  )
  sieve_record_decision(s, "model_choice", "Which model?", "A", "Fit, and \"meaning\", too.\nSecond line.")
  sieve_record_decision(s, "factor_retention", "How many factors?", 2L, "Replication.")
  tab <- shiny::isolate(sieve_decisions_table(s))

  expect_identical(names(tab), c(
    "id", "topic", "question", "choice", "rationale", "evidence", "supersedes",
    "superseded_by", "status", "dataset_id", "data_key", "meta_key",
    "siever_version", "recorded_at"
  ))
  expect_identical(dim(tab), c(3L, 14L))
  expect_true(all(vapply(tab, is.character, logical(1))))
  expect_identical(tab$id, c("D1", "D2", "D3"))
  expect_identical(tab$choice, c("3", "A", "2"))
  expect_identical(tab$evidence, c("parallel analysis: 3; MAP: 2", NA, NA))
  expect_identical(tab$supersedes, c(NA, NA, "D1"))
  expect_identical(tab$superseded_by, c("D3", NA, NA))
  expect_identical(tab$status, c("superseded", "current", "current"))
  expect_identical(unique(tab$dataset_id), "D1")
  expect_identical(unique(tab$siever_version), sieve_version())
  # The table can also be built from the list of records itself.
  expect_identical(sieve_decisions_table(decisions_of(s)), tab)

  # It survives export: the file read back carries the same records.
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  utils::write.csv(tab, f, row.names = FALSE)
  back <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character", na.strings = "NA")
  expect_identical(back$id, tab$id)
  expect_identical(back$rationale, tab$rationale)
  expect_identical(back$superseded_by, tab$superseded_by)
})

test_that("each record is logged to the reproducible code as comments only", {
  s <- decision_state()
  rationale <- "Three factors.\nstop('this line must never run')\r\n  x <- 1 # nor this"
  rec <- sieve_record_decision(s, "factor_retention", "How many factors?", 3L, rationale,
    evidence = c("parallel analysis: 3", "MAP: 2")
  )
  log <- shiny::isolate(s$codelog)
  expect_match(log[1], "^# \\[1\\] Decision D1 \\(factor_retention\\): 3$")
  block <- sieve_decision_code(rec)
  expect_true(all(block %in% log))
  # Whatever the researcher typed, every line of the block is a comment, so
  # the block adds no expression to the script.
  expect_true(all(startsWith(block, "#")))
  expect_length(parse(text = block), 0L)
  expect_length(parse(text = log), 0L)
  joined <- paste(block, collapse = "\n")
  expect_match(joined, "Decision D1: factor_retention", fixed = TRUE)
  expect_match(joined, "stop('this line must never run')", fixed = TRUE)
  expect_match(joined, "MAP: 2", fixed = TRUE)
  expect_match(joined, "dataset D1", fixed = TRUE)
  expect_match(joined, sieve_version(), fixed = TRUE)
  expect_match(joined, "not computed by the code", fixed = TRUE)

  # A revision is logged as its own step and says what it replaces; two
  # identical decisions in a row are still two records and two steps.
  sieve_record_decision(s, "factor_retention", "How many factors?", 3L, rationale)
  sieve_record_decision(s, "factor_retention", "How many factors?", 3L, rationale)
  log <- shiny::isolate(s$codelog)
  expect_identical(sum(grepl("^# \\[[0-9]+\\] Decision ", log)), 3L)
  expect_true(any(grepl("^# \\[3\\] Decision D3 \\(factor_retention\\): 3, replacing D2$", log)))
  expect_true(any(grepl("^# Replaces: +D2", log)))
  expect_length(parse(text = log), 0L)
})

# ---- Blank rationales, and the key written with each record ---------------

test_that("a rationale that only looks like text is recorded as none given, and one in any script is kept", {
  # A blank rationale is recorded as "no rationale given" rather than
  # refused; sieve_has_content() decides what counts as blank.
  # The characters are built from their code points, so the test reads the
  # same in every locale: a non-breaking space, an em space, a thin space,
  # a zero width space, a byte order mark, an em dash, four Han characters,
  # and a word with an accented letter.
  nbsp <- intToUtf8(160L)
  em_space <- intToUtf8(8195L)
  thin_space <- intToUtf8(8201L)
  zero_width <- intToUtf8(8203L)
  byte_order <- intToUtf8(65279L)
  dash <- intToUtf8(8212L)
  han <- c(19977L, 20010L, 22240L, 23376L)
  accented <- c(99L, 97L, 102L, 233L)
  # trimws() leaves a non-breaking space in place, so a check that trims and
  # tests for an empty string would take a rationale made of one as content,
  # and a single period as well.
  expect_identical(utf8ToInt(trimws(nbsp)), 160L)
  blank <- c(
    nbsp, paste0(em_space, thin_space), paste0(zero_width, byte_order),
    paste0(" ", nbsp, "\t\n"), ".", "---", "?!", dash, ""
  )
  expect_false(any(vapply(blank, sieve_has_content, logical(1))))
  expect_true(all(vapply(
    c("ok", "3", intToUtf8(han), intToUtf8(accented), "MAP said 2."),
    sieve_has_content, logical(1)
  )))
  expect_false(sieve_has_content(NULL))
  expect_false(sieve_has_content(NA_character_))
  expect_false(sieve_has_content(c("one", "two")))
  expect_false(sieve_has_content(3))

  s <- decision_state()
  for (why in blank) {
    r <- sieve_record_decision(s, "factor_retention", "How many factors?", 3L, why)
    expect_identical(r$rationale, sieve_no_rationale())
  }
  expect_length(decisions_of(s), length(blank))

  # Spaces of every kind are trimmed from the ends, and only from the ends.
  rec <- sieve_record_decision(
    s, "factor_retention", "How many factors?", 3L,
    paste0(nbsp, " Three, as theorized.", em_space, "\n")
  )
  expect_identical(rec$rationale, "Three, as theorized.")
  expect_identical(utf8ToInt(sieve_trim(paste0("  a", nbsp, "b  "))), c(97L, 160L, 98L))
  expect_identical(sieve_trim(paste0(nbsp, thin_space)), "")
  expect_identical(sieve_trim("  plain  "), "plain")
  other <- sieve_record_decision(s, "model_choice", "Which model?", "A", intToUtf8(han))
  expect_identical(utf8ToInt(other$rationale), han)
})

test_that("the key written with a record makes a later edit of the record visible", {
  s <- decision_state()
  r1 <- sieve_record_decision(s, "factor_retention", "How many factors?", 3L, "Theory.",
    evidence = c("parallel analysis: 3", "MAP: 2")
  )
  r2 <- sieve_record_decision(s, "model_choice", "Which model?", "A", "Fit and meaning.")
  log <- shiny::isolate(s$codelog)
  # The key is a content hash of the record and is in the script, in the
  # record's comment block; the record and the table are as they were.
  key1 <- sieve_decision_key(r1)
  expect_match(key1, "^[0-9a-f]{32}$")
  expect_true(any(log == paste("# Record key:", key1)))
  expect_true(any(log == paste("# Record key:", sieve_decision_key(r2))))
  expect_false("key" %in% names(r1))
  expect_length(parse(text = log), 0L)
  # The key moves with every part of the record.
  for (field in c("id", "topic", "question", "choice", "rationale", "evidence", "supersedes", "version", "time")) {
    changed <- r1
    changed[[field]] <- if (identical(field, "choice")) 9L else "changed"
    expect_false(identical(sieve_decision_key(changed), key1), info = field)
  }
  moved <- r1
  moved$revision$meta_key <- "0"
  expect_false(identical(sieve_decision_key(moved), key1))
  expect_identical(sieve_decision_key(r1), key1)

  check <- shiny::isolate(sieve_decisions_check(s))
  expect_identical(check$id, c("D1", "D2"))
  expect_identical(check$key, check$recorded_key)
  expect_identical(check$intact, c(TRUE, TRUE))

  # Nothing in R prevents an assignment into the list of records; the check
  # must notice one.
  held <- decisions_of(s)
  held[["D1"]]$choice <- 9L
  s$decisions <- held
  check <- shiny::isolate(sieve_decisions_check(s))
  expect_identical(check$intact, c(FALSE, TRUE))
  expect_identical(check$recorded_key[1], key1) # the script still holds the record as made
  expect_false(identical(check$key[1], key1))

  # A superseding record has its own block and key; both stay checkable.
  # (A record is appended only while every earlier record is what was
  # written, so D1 is put back as written first; the refusal itself is
  # tested below.)
  held[["D1"]] <- r1
  s$decisions <- held
  r3 <- sieve_record_decision(s, "model_choice", "Which model?", "B", "A did not replicate.")
  check <- shiny::isolate(sieve_decisions_check(s))
  expect_identical(check$intact, c(TRUE, TRUE, TRUE))
  expect_identical(r3$supersedes, "D2")
  # Records without a key in the script cannot be checked, and say so.
  none <- sieve_decisions_check(list(D1 = r1))
  expect_true(is.na(none$intact))
  expect_identical(nrow(sieve_decisions_check(list())), 0L)
})

# ---- No new record while an earlier one has changed -----------------------

test_that("a new record is refused while an earlier record is not what was written", {
  s <- decision_state()
  r1 <- sieve_record_decision(s, "factor_retention", "How many factors?", 3L, "Three, as theorized.")
  held <- decisions_of(s)
  held[["D1"]]$choice <- 9L
  s$decisions <- held
  log_before <- shiny::isolate(s$codelog)
  # The chain is not extended on an altered link, on any topic.
  expect_error(
    sieve_record_decision(s, "factor_retention", "How many factors?", 2L, "Two, on reflection."),
    class = "sieve_decisions_altered"
  )
  err <- tryCatch(sieve_record_decision(s, "model_choice", "Which model?", "A", "Fit and meaning."),
    error = function(e) e
  )
  expect_s3_class(err, "sieve_decisions_altered")
  expect_identical(err$altered, "D1")
  expect_match(conditionMessage(err), "Decision record D1 no longer matches", fixed = TRUE)
  expect_match(conditionMessage(err), "nothing was recorded", fixed = TRUE)
  expect_length(decisions_of(s), 1L)
  expect_identical(shiny::isolate(s$codelog), log_before)
  # Put back as it was written, the chain continues.
  held[["D1"]] <- r1
  s$decisions <- held
  r2 <- sieve_record_decision(s, "factor_retention", "How many factors?", 2L, "Two, on reflection.")
  expect_identical(r2$supersedes, "D1")
  expect_identical(shiny::isolate(sieve_decisions_check(s))$intact, c(TRUE, TRUE))
  # Two altered records are both named.
  held <- decisions_of(s)
  held[["D1"]]$rationale <- "edited"
  held[["D2"]]$rationale <- "edited"
  s$decisions <- held
  err <- tryCatch(sieve_record_decision(s, "factor_retention", "How many factors?", 1L, "One."),
    error = function(e) e
  )
  expect_identical(err$altered, c("D1", "D2"))
  expect_match(conditionMessage(err), "Decision records D1, D2 no longer match", fixed = TRUE)
})

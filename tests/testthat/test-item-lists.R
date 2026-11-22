# Selecting many items at once on the Ingest tab: a pasted list of items to
# exclude, to include, or to include alone, and the originals of the
# reverse-coded copies a file holds beside them.
#
# Author: Ken Kelley

il_meta <- function() {
  data.frame(
    item = c("q1", "q2", "Q3", "q4", "q5"),
    label = c("Visual 1", "Visual 2", "Text one", "Text  two", "Speed"),
    wording = c("I like maps", "I draw well", NA, "I read daily", "I work fast"),
    status = "active",
    stringsAsFactors = FALSE
  )
}

test_that("a pasted list is split at line breaks and tabs, without quotes, and at commas and semicolons when matched", {
  expect_identical(
    sieve_item_list_entries("q1\nq2\r\n\tq3, \"q4\";  'q5'\n\n"),
    c("q1", "q2", "q3, \"q4\";  'q5")
  )
  expect_identical(sieve_item_list_entries("\u201cq4\u201d\n`q5`"), c("q4", "q5"))
  m <- data.frame(item = paste0("q", 1:5), status = "active", stringsAsFactors = FALSE)
  found <- sieve_match_item_list(sieve_item_list_entries("q1\nq2\r\n\tq3, \"q4\";  'q5'\n\n"), m)
  expect_identical(found$items, paste0("q", 1:5))
  expect_identical(sieve_item_list_entries(c("q1", "q2 q3")), c("q1", "q2 q3"))
  expect_identical(sieve_item_list_entries(""), character(0))
  expect_identical(sieve_item_list_entries(NULL), character(0))
})

test_that("entries match a Variable Name, then an Item Name, then either ignoring case, then the wording", {
  m <- il_meta()
  found <- sieve_match_item_list(c("q1", "Visual 2", "q3", "text two", "i work FAST"), m)
  expect_identical(found$items, c("q1", "q2", "Q3", "q4", "q5"))
  expect_identical(found$matches$item, c("q1", "q2", "Q3", "q4", "q5"))
  expect_length(found$unmatched, 0L)
  expect_length(found$ambiguous, 0L)
})

test_that("a list on one line separated by spaces is read as names, and a name found nowhere is reported", {
  m <- il_meta()
  found <- sieve_match_item_list(c("q1 q2 q5", "q9", "q1 q9"), m)
  expect_identical(found$items, c("q1", "q2", "q5"))
  expect_identical(found$unmatched, c("q9", "q1 q9"))
})

test_that("a wording that holds a comma matches as a whole before it is split", {
  m <- data.frame(
    item = c("q1", "q2", "q3"), status = "active",
    wording = c("I feel nervous", "I feel nervous, even at home", "I sleep well"),
    stringsAsFactors = FALSE
  )
  expect_identical(sieve_match_item_list(sieve_item_list_entries("I feel nervous, even at home"), m)$items, "q2")
  found <- sieve_match_item_list(sieve_item_list_entries("q3, i feel nervous"), m)
  expect_identical(found$items, c("q1", "q3"))
})

test_that("an entry that names two items at its deciding step is left alone and reported", {
  m <- il_meta()
  m$label[2] <- "Visual 1"
  found <- sieve_match_item_list(c("Visual 1", "q5"), m)
  expect_identical(found$items, "q5")
  expect_identical(found$ambiguous, "Visual 1")
  # An exact Variable Name decides before the Item Names are read.
  m$label[5] <- "q1"
  expect_identical(sieve_match_item_list("q1", m)$items, "q1")
})

il_data <- function() {
  set.seed(1)
  n <- 60L
  q1 <- sample(1:5, n, replace = TRUE)
  q3 <- sample(1:5, n, replace = TRUE)
  q4 <- sample(1:5, n, replace = TRUE)
  q6 <- sample(1:5, n, replace = TRUE)
  data.frame(
    q1 = q1, q1_r = 6 - q1,
    q2 = sample(1:5, n, replace = TRUE),
    q3 = q3, rQ3 = 6 - q3,
    q4 = q4, q4.rev = q4,
    q6 = q6, Q6_REVERSED = 6 - q6
  )
}

il_ledger <- function(d) {
  data.frame(item = names(d), status = "active", reverse = FALSE, stringsAsFactors = FALSE)
}

test_that("an original and its reverse-coded copy are paired by name and proposed when their values run opposite", {
  d <- il_data()
  m <- il_ledger(d)
  op <- sieve_reversed_originals(d, m)
  go <- op[op$proposed, ]
  expect_setequal(go$original, c("q1", "q3", "q6"))
  expect_identical(go$copy[match(c("q1", "q3", "q6"), go$original)], c("q1_r", "rQ3", "Q6_REVERSED"))
  expect_equal(go$r, rep(-1, 3L))
  expect_identical(go$n, rep(60L, 3L))
  # A copy named as reversed whose values run with the original's is not
  # proposed, and the reason says so.
  held <- op[!op$proposed, ]
  expect_identical(held$original, "q4")
  expect_match(held$reason, "do not run opposite", fixed = TRUE)
  # An item with no copy (q2) is in no pair.
  expect_false("q2" %in% c(op$original, op$copy))
})

test_that("a pair with one of the two reverse scored here runs opposite when its values now run together", {
  d <- il_data()
  m <- il_ledger(d)
  # q1 reverse scored in this session: its values now run with q1_r's.
  d$q1 <- 6 - d$q1
  m$reverse[m$item == "q1"] <- TRUE
  op <- sieve_reversed_originals(d, m)
  row <- op[op$original == "q1", ]
  expect_true(row$flipped)
  expect_equal(row$r, 1)
  expect_true(row$proposed)
})

test_that("a pair is not proposed when the original is already excluded or the copy is", {
  d <- il_data()
  m <- il_ledger(d)
  m$status[m$item == "q1"] <- "parked"
  m$status[m$item == "rQ3"] <- "parked"
  op <- sieve_reversed_originals(d, m)
  expect_false(op$proposed[op$original == "q1"])
  expect_match(op$reason[op$original == "q1"], "already excluded", fixed = TRUE)
  expect_false(op$proposed[op$original == "q3"])
  expect_match(op$reason[op$original == "q3"], "the copy rQ3 is excluded", fixed = TRUE)
  expect_identical(nrow(sieve_reversed_originals(d[0], m[0, ])), 0L)
})

il_construct_data <- function() {
  set.seed(1)
  f <- stats::rnorm(200)
  one <- function() pmin(5, pmax(1, round(3 + f + stats::rnorm(200, 0, 0.7))))
  d <- data.frame(v1 = one(), v2 = one(), v3 = one(), v4 = one(), v5 = 6 - one())
  d$v5_r <- 6 - d$v5
  d$v1_r <- 6 - d$v1
  d
}

test_that("with a construct assigned, the keyed item of a pair is read from the construct's other items", {
  d <- il_construct_data()
  m <- data.frame(item = names(d), status = "active", construct = "Visual", reverse = FALSE, stringsAsFactors = FALSE)
  op <- sieve_reversed_originals(d, m)
  # v5 is worded against the construct: its reverse-coded copy is the keyed one.
  v5 <- op[op$original == "v5", ]
  expect_identical(v5$keyed, "copy")
  expect_true(v5$proposed)
  expect_true(v5$r_copy > 0 && v5$r_original < 0)
  expect_identical(v5$construct, "Visual")
  # v1 is worded with the construct: the original is the keyed one, so the
  # pair is not proposed, and the reason says the copy is the one to exclude.
  v1 <- op[op$original == "v1", ]
  expect_identical(v1$keyed, "original")
  expect_false(v1$proposed)
  expect_match(v1$reason, "v1, the original, is the one keyed with Visual", fixed = TRUE)
  expect_match(v1$reason, "so the copy is the one to exclude", fixed = TRUE)
  # One member's correlation decides when the other's cannot be read.
  m1 <- m
  m1$construct[m1$item == "v5"] <- "Unassigned"
  v5 <- sieve_reversed_originals(d, m1)
  v5 <- v5[v5$original == "v5", ]
  expect_identical(v5$keyed, "copy")
  expect_true(v5$proposed)
  # Without a construct, the name decides for both.
  m$construct <- "Unassigned"
  op <- sieve_reversed_originals(d, m)
  expect_true(all(op$proposed))
  expect_true(all(is.na(op$keyed)))
})

test_that("two items that both run with their construct are not proposed as an original and its copy", {
  set.seed(2)
  f <- stats::rnorm(200)
  one <- function() pmin(5, pmax(1, round(3 + f + stats::rnorm(200, 0, 0.7))))
  g <- stats::rnorm(200)
  two <- function() pmin(5, pmax(1, round(3 + g + stats::rnorm(200, 0, 0.7))))
  d <- data.frame(e1 = one(), e2 = one(), e3 = one(), re1 = two(), re2 = two(), re3 = two())
  m <- data.frame(
    item = names(d), status = "active", construct = rep(c("E", "RE"), each = 3L),
    reverse = FALSE, stringsAsFactors = FALSE
  )
  op <- sieve_reversed_originals(d, m)
  expect_false(any(op$proposed))
  expect_true(all(grepl("both correlate positively", op$reason, fixed = TRUE) |
    grepl("do not run opposite", op$reason, fixed = TRUE)))
})

test_that("a second copy of one original stays out of the construct mean the first is read against", {
  set.seed(3)
  f <- stats::rnorm(300)
  one <- function(s) pmin(5, pmax(1, round(3 + f + stats::rnorm(300, 0, s))))
  d <- data.frame(v1 = one(0.7), v2 = one(0.7), q5 = one(3))
  d$q5_r <- 6 - d$q5
  d$rev_q5 <- 6 - d$q5
  m <- data.frame(item = names(d), status = "active", construct = "V", reverse = FALSE, stringsAsFactors = FALSE)
  op <- sieve_reversed_originals(d, m)
  rest <- rowMeans(d[, c("v1", "v2")])
  expect_equal(op$r_original[op$copy == "q5_r"], stats::cor(d$q5, rest))
  expect_identical(unique(op$keyed[op$original == "q5"]), if (stats::cor(d$q5, rest) > 0) "original" else "copy")
})

test_that("a chain of names keeps the first copy and does not exclude it as an original", {
  d <- data.frame(q5 = c(1, 2, 3, 4, 5, 2, 3, 4, 1, 5))
  d$q5_r <- 6 - d$q5
  d$q5_r_r <- 6 - d$q5_r
  m <- data.frame(item = names(d), status = "active", reverse = FALSE, stringsAsFactors = FALSE)
  op <- sieve_reversed_originals(d, m)
  expect_true(op$proposed[op$original == "q5" & op$copy == "q5_r"])
  chained <- op[op$original == "q5_r", ]
  expect_false(chained$proposed)
  expect_identical(chained$reason, "q5_r is itself the reverse-coded copy of q5")
})

test_that("the Ingest grid excludes, includes, includes only a pasted list, and undoes it", {
  d <- il_data()
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(d, path, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "items.csv", datapath = path))
    session$setInputs(load = 1L)
    m <- shiny::isolate(state$meta)
    expect_true(all(m$status == "active"))
    status_of <- function(x) shiny::isolate(state$meta)$status[match(x, shiny::isolate(state$meta)$item)]

    session$setInputs(list_text = "q2\nq4, nothing_here", list_exclude = 1L)
    expect_identical(status_of(c("q2", "q4")), c("parked", "parked"))
    expect_identical(shiny::isolate(state$meta)$exclude_reason[match("q2", m$item)], "excluded by a pasted list")
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"), "Exclude the 2 items of a pasted list", fixed = TRUE)
    expect_match(paste(as.character(output$list_result$html), collapse = ""), "nothing_here", fixed = TRUE)

    session$setInputs(list_text = "q2", list_include = 1L)
    expect_identical(status_of(c("q2", "q4")), c("active", "parked"))

    session$setInputs(list_text = "q1_r\trQ3", list_only = 1L)
    expect_identical(sort(shiny::isolate(state$meta)$item[shiny::isolate(state$meta)$status == "active"]), c("q1_r", "rQ3"))

    session$setInputs(list_undo = 1L)
    expect_identical(status_of(c("q1", "q2", "q4")), c("active", "active", "parked"))

    # An item changed again after the list keeps that later change.
    session$setInputs(list_text = "q1\nq2", list_exclude = 2L)
    session$setInputs(cell_edit = list(item = "q2", field = "status", value = "Included"))
    session$setInputs(cell_edit = list(item = "q2", field = "status", value = "Excluded"))
    session$setInputs(list_undo = 2L)
    expect_identical(status_of(c("q1", "q2")), c("active", "parked"))
    expect_match(paste(as.character(output$list_result$html), collapse = ""), "Changed again after the list", fixed = TRUE)

    # A list applied to one study is not undone in the next.
    session$setInputs(list_text = "q3", list_exclude = 3L)
    session$setInputs(load = 2L)
    session$setInputs(load_replace = 1L)
    expect_true(all(shiny::isolate(state$meta)$status == "active"))
    session$setInputs(list_undo = 3L)
    expect_true(all(shiny::isolate(state$meta)$status == "active"))
    expect_match(paste(as.character(output$list_result$html), collapse = ""), "There is no pasted list to undo.", fixed = TRUE)
  })
})

test_that("the Ingest grid excludes every original saved beside its reverse-coded copy in one click", {
  d <- il_data()
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(d, path, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "items.csv", datapath = path))
    session$setInputs(load = 1L)
    note <- paste(as.character(output$originals_note$html), collapse = "")
    expect_match(note, "Exclude the 3 originals, keep the reverse-coded copies", fixed = TRUE)
    expect_match(note, "so the names decide", fixed = TRUE)
    # The pairs the note proposes are not also listed as the same item twice.
    copies <- paste(as.character(output$copies_note$html), collapse = "")
    expect_false(grepl("q1 and q1_r", copies, fixed = TRUE))
    session$setInputs(originals_exclude = 1L)
    m <- shiny::isolate(state$meta)
    expect_identical(m$status[match(c("q1", "q3", "q6"), m$item)], rep("parked", 3L))
    expect_identical(m$status[match(c("q1_r", "rQ3", "Q6_REVERSED", "q4", "q4.rev"), m$item)], rep("active", 5L))
    expect_identical(m$exclude_reason[match("q1", m$item)], "the original of the reverse-coded copy q1_r")
  })
})

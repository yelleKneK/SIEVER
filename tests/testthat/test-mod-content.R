# Tests for the Content Validity module. Content evidence is pre-data
# evidence, so most of these run with no data loaded at all; the ledger
# wiring is the exception and is driven against the bundled nine test
# example. Every statistic is pinned against a direct
# DMAR::content_validity_index() call so the app and the package cannot drift.

skip_if_not_installed("DMAR")
skip_if_not_installed("shiny")

content_state <- function(confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

write_ratings_csv <- function(m, path) {
  df <- data.frame(
    item = rownames(m), as.data.frame(m, check.names = FALSE),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  utils::write.csv(df, path, row.names = FALSE, na = "")
  path
}

test_that("an uploaded ratings CSV is parsed back exactly as it was written", {
  m <- sieve_content_demo_ratings()
  m[3, 5] <- NA # a missing rating must survive the round trip
  path <- write_ratings_csv(m, tempfile(fileext = ".csv"))
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(file = list(datapath = path, name = "panel.csv"))
    r <- ratings_in()
    expect_true(isTRUE(r$ok))
    expect_identical(rownames(r$ratings), rownames(m))
    expect_identical(colnames(r$ratings), colnames(m))
    expect_equal(unname(r$ratings), unname(m))
    expect_true(is.na(r$ratings[3, 5]))
  })
})

test_that("I-CVI, the interval, the modified kappa, and CVR match DMAR to machine precision", {
  m <- sieve_content_demo_ratings()
  path <- write_ratings_csv(m, tempfile(fileext = ".csv"))
  ref <- DMAR::content_validity_index(m,
    relevant = c(3, 4), essential = NULL,
    conf_level = 0.95
  )
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(file = list(datapath = path, name = "panel.csv"))
    d <- cvi()
    expect_identical(d$item, ref$item)
    expect_identical(d$n_experts, ref$n_experts)
    expect_identical(d$n_relevant, ref$n_relevant)
    expect_identical(d$i_cvi, ref$i_cvi)
    expect_identical(d$ci_lower, ref$ci_lower)
    expect_identical(d$ci_upper, ref$ci_upper)
    expect_identical(d$kappa, ref$kappa)
    expect_identical(d$cvr, ref$cvr)
    expect_identical(attr(d, "s_cvi_ave"), attr(ref, "s_cvi_ave"))
    expect_identical(attr(d, "s_cvi_ua"), attr(ref, "s_cvi_ua"))
    # The scale level table carries the attributes through unrounded.
    sc <- scvi()
    expect_identical(sc$value[1], attr(ref, "s_cvi_ave"))
    expect_identical(sc$value[2], attr(ref, "s_cvi_ua"))
  })
})

test_that("a non default relevance dichotomization and confidence level reach DMAR", {
  m <- sieve_content_demo_ratings()
  path <- write_ratings_csv(m, tempfile(fileext = ".csv"))
  ref <- DMAR::content_validity_index(m,
    relevant = 4, essential = NULL,
    conf_level = 0.90
  )
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = "4", conf_level = 0.90)
    session$setInputs(file = list(datapath = path, name = "panel.csv"))
    d <- cvi()
    expect_identical(d$i_cvi, ref$i_cvi)
    expect_identical(d$ci_lower, ref$ci_lower)
    expect_identical(d$ci_upper, ref$ci_upper)
    expect_identical(attr(d, "conf_level"), 0.90)
  })
})

test_that("the demonstration ratings load, compute, and are labeled as illustrative", {
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(demo = 1)
    r <- ratings_in()
    expect_true(isTRUE(r$ok))
    expect_equal(dim(r$ratings), c(9L, 6L))
    expect_match(r$source, "illustrative")
    d <- cvi()
    ref <- DMAR::content_validity_index(sieve_content_demo_ratings(),
      relevant = c(3, 4), conf_level = 0.95
    )
    expect_identical(d$i_cvi, ref$i_cvi)
    # No cutoff grades an I-CVI or a CVR: the table shows the values
    # uncolored, with no flag column,
    # and the conventions are named with their sources as context.
    tbl <- paste(as.character(unlist(output$cvi_tbl)), collapse = " ")
    expect_false(grepl("#f8e3e3|Inspect and reason|icvi_flag|cvr_flag", tbl))
    expect_match(tbl, "CVR (relevance ratings)", fixed = TRUE)
    note <- paste(as.character(unlist(output$scvi_note)), collapse = " ")
    expect_match(note, "Polit and Beck (2006) proposed 0.90 for S-CVI/Ave as a convention; SIEVER applies no cutoff.", fixed = TRUE)
    expect_false(grepl("against the conventional", note, fixed = TRUE))
  })
  # The demonstration item names are the bundled nine test example's items,
  # which is what makes the ledger demonstration possible.
  expect_identical(
    rownames(sieve_content_demo_ratings()),
    sieve_hs9_fixture()$item_cols
  )
})

test_that("the item level sentence names Lynn's convention and states the CVR by its definition, attributing no rule to Lawshe", {
  # A CVR above zero means more than half the panel called the item
  # essential by the ratio's algebra alone, so the sentence states it as the
  # definition and attributes no rule to Lawshe (1975).
  x <- sieve_content_convention_text("item")
  expect_false(grepl("Lawshe", x, fixed = TRUE))
  expect_match(x, "a CVR above zero means, by its definition, that more than half the panel called the item essential", fixed = TRUE)
  expect_match(x, "Lynn (1986), as Polit and Beck (2006) summarize her, proposed an I-CVI of 1.00 with five or fewer experts", fixed = TRUE)
  expect_match(x, "SIEVER applies no cutoff", fixed = TRUE)
})

test_that("the ledger note is written for matching item names", {
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(demo = 1)
    mm <- matched()
    expect_true(all(!is.na(mm$ledger_item)))
    before <- state$meta$note
    expect_true(all(is.na(before)))
    session$setInputs(keep_all_matches = 1)
    session$setInputs(record_note = 1)
    after <- state$meta
    expect_true(all(grepl("^I-CVI = ", after$note)))
    # The note carries the interval, and the number in it is the DMAR value.
    ref <- DMAR::content_validity_index(sieve_content_demo_ratings(),
      relevant = c(3, 4), conf_level = 0.95
    )
    idx <- match(ref$item, after$item)
    expect_identical(
      after$note[idx],
      sprintf(
        "I-CVI = %.3f (%d experts, 95%% CI [%.3f, %.3f])",
        ref$i_cvi, as.integer(ref$n_experts), ref$ci_lower, ref$ci_upper
      )
    )
    # Pressing it a second time replaces rather than stacks the note.
    session$setInputs(keep_all_matches = 2)
    session$setInputs(record_note = 2)
    expect_identical(state$meta$note[idx], after$note[idx])
  })
})

test_that("excluding the selected items sets status to parked without removing the row", {
  st <- content_state()
  test_frame <- environment()
  warned <- new.env()
  warned$n <- 0L
  local_mocked_bindings(sieve_notify = function(message, type = "message", ...) {
    if (!identical(type, "message")) warned$n <- warned$n + 1L
    invisible(NULL)
  }, .env = test_frame)
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(demo = 1)
    n_before <- nrow(state$meta)
    picked <- c(7L, 9L)
    chosen <- matched()$ledger_item[picked]
    expect_true(all(state$meta$status == "active"))
    session$setInputs(keep_all_matches = 1)
    # No row selected: nothing is excluded, and no cutoff selects any (the
    # button does not exclude the items below Lynn's value).
    session$setInputs(park_selected = 1)
    expect_null(park_pending())
    expect_true(all(state$meta$status == "active"))
    # The refusal is written under the buttons and stays there, not in a
    # warning notification that vanishes after six seconds.
    note <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(output$action_note$html), collapse = " ")))
    expect_match(note, "Select the rows of the items to exclude in the table below", fixed = TRUE)
    expect_match(as.character(output$action_note$html), "sieve-status-warning", fixed = TRUE)
    expect_identical(warned$n, 0L)
    # (The button lists the items and the exclusion waits for
    # confirmation.)
    session$setInputs(ledger_tbl_rows_selected = picked)
    # A new selection clears it.
    expect_false(grepl("Select the rows", paste(as.character(unlist(output$action_note)), collapse = " "), fixed = TRUE))
    session$setInputs(park_selected = 2)
    expect_identical(park_pending()$ledger_item, chosen)
    expect_true(all(state$meta$status == "active"))
    session$setInputs(park_selected_confirm = 1)
    meta <- state$meta
    expect_identical(nrow(meta), n_before)
    expect_true(all(meta$status[meta$item %in% chosen] == "parked"))
    expect_true(all(meta$status[!(meta$item %in% chosen)] == "active"))
    # Nothing is destroyed: the wording and the ledger row survive.
    expect_true(all(chosen %in% meta$item))
  })
  expect_identical(sieve_content_selected_rows(c(3, 1, 3, 12, NA), 9L), c(1L, 3L))
  expect_identical(sieve_content_selected_rows(NULL, 9L), integer(0))
})

test_that("a bad upload produces the specific actionable message rather than a crash", {
  st <- content_state()

  # Non numeric cells.
  p1 <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      item = c("a", "b"), expert_1 = c(4, 3),
      expert_2 = c("n/a", "4"), stringsAsFactors = FALSE
    ),
    p1,
    row.names = FALSE
  )
  r1 <- sieve_parse_ratings(p1, "bad_text.csv")
  expect_false(r1$ok)
  expect_match(r1$message, "are not numbers")
  expect_match(r1$message, "expert_2", fixed = TRUE)

  # Only one expert column.
  p2 <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(item = c("a", "b"), expert_1 = c(4, 3)),
    p2,
    row.names = FALSE
  )
  r2 <- sieve_parse_ratings(p2, "one_expert.csv")
  expect_false(r2$ok)
  expect_match(r2$message, "at least 2 expert columns")

  # Ratings off the 4 point scale.
  p3 <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      item = c("a", "b"), expert_1 = c(4, 7),
      expert_2 = c(0, 3)
    ),
    p3,
    row.names = FALSE
  )
  r3 <- sieve_parse_ratings(p3, "off_scale.csv")
  expect_false(r3$ok)
  expect_match(r3$message, "outside the 4 point relevance scale")
  expect_match(r3$message, "0, 7", fixed = TRUE)

  # The module surfaces the message and does not compute.
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(file = list(datapath = p3, name = "off_scale.csv"))
    r <- ratings_in()
    expect_false(r$ok)
    expect_match(
      as.character(output$parse_note$html),
      "outside the 4 point relevance scale"
    )
    expect_error(cvi(), class = "shiny.silent.error")
  })
})

test_that("a numeric first column of item identifiers is not counted as an expert", {
  # Deciding on storage type alone would count an identifier column as one
  # more expert and fold the identifiers into the relevance ratings, which
  # changes every I-CVI and changes the expert count that selects the panel
  # size dependent Lynn (1986) criterion.
  m <- rbind(
    c(4, 4, 4, 3, 4),
    c(4, 3, 4, 4, 3),
    c(2, 3, 1, 2, 3),
    c(1, 2, 3, 2, 2)
  )
  dimnames(m) <- list(paste0("q", 1:4), paste0("expert_", 1:5))
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      item = seq_len(nrow(m)), as.data.frame(m, check.names = FALSE),
      check.names = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )

  # What deciding on storage type alone would produce: six experts instead
  # of five, and I-CVIs that are wrong.
  raw <- utils::read.csv(path, check.names = FALSE)
  as_expert <- as.matrix(raw)
  storage.mode(as_expert) <- "numeric"
  wrong <- DMAR::content_validity_index(as_expert, relevant = c(3, 4))
  right <- DMAR::content_validity_index(m, relevant = c(3, 4))
  expect_equal(unique(wrong$n_experts), 6)
  expect_equal(unique(right$n_experts), 5)
  expect_false(isTRUE(all.equal(wrong$i_cvi, right$i_cvi)))

  # The header settles it, so the identifiers name the items and every rating
  # column is an expert.
  res <- sieve_parse_ratings(path, "numbered_items.csv")
  expect_true(res$ok)
  expect_true(res$has_names)
  expect_identical(res$item_source, "column")
  expect_identical(dim(res$ratings), c(4L, 5L))
  expect_identical(colnames(res$ratings), colnames(m))
  expect_identical(rownames(res$ratings), as.character(1:4))
  expect_equal(unname(res$ratings), unname(m))
  # A recognized header is positive evidence, so there is nothing to disclose.
  expect_true(is.na(res$note))

  got <- DMAR::content_validity_index(res$ratings, relevant = c(3, 4))
  expect_identical(got$i_cvi, right$i_cvi)
  expect_identical(got$n_experts, right$n_experts)
  expect_identical(got$kappa, right$kappa)
  expect_identical(attr(got, "s_cvi_ave"), attr(right, "s_cvi_ave"))
})

test_that("items numbered past the rating scale do not look like off scale ratings", {
  # With nine items the identifiers run to 9, so reading that column as an
  # expert would reject the file for holding ratings outside the 4 point
  # scale, blaming the ratings for a column identification problem.
  m <- sieve_content_demo_ratings()
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      item = seq_len(nrow(m)), as.data.frame(m, check.names = FALSE),
      check.names = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )
  res <- sieve_parse_ratings(path, "numbered_items.csv")
  expect_true(res$ok)
  expect_identical(dim(res$ratings), c(9L, 6L))
  expect_equal(unname(res$ratings), unname(m))
})

test_that("an ambiguous numeric first column is disclosed rather than decided in silence", {
  m <- sieve_content_demo_ratings()

  # No item column at all: the first column is one expert's ratings, its values
  # repeat, and it stays an expert column with the reading stated.
  p1 <- tempfile(fileext = ".csv")
  utils::write.csv(as.data.frame(m, check.names = FALSE), p1,
    row.names = FALSE, na = ""
  )
  r1 <- sieve_parse_ratings(p1, "experts_only.csv")
  expect_true(r1$ok)
  expect_false(r1$has_names)
  expect_identical(dim(r1$ratings), c(9L, 6L))
  expect_identical(r1$item_source, "numbered")
  expect_false(is.na(r1$note))
  expect_match(r1$note, "read as an expert")
  expect_match(r1$note, "'item'", fixed = TRUE)
  expect_identical(
    DMAR::content_validity_index(r1$ratings, relevant = c(3, 4))$i_cvi,
    DMAR::content_validity_index(m, relevant = c(3, 4))$i_cvi
  )

  # Distinct numeric identifiers under a header the app does not recognize:
  # read as item names, because a rating column on the 1 to 4 scale cannot be
  # distinct across nine items, and that reading is stated as well.
  p2 <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      q_no = 101:109, as.data.frame(m, check.names = FALSE),
      check.names = FALSE
    ),
    p2,
    row.names = FALSE, na = ""
  )
  r2 <- sieve_parse_ratings(p2, "q_no.csv")
  expect_true(r2$ok)
  expect_true(r2$has_names)
  expect_identical(dim(r2$ratings), c(9L, 6L))
  expect_identical(rownames(r2$ratings), as.character(101:109))
  expect_match(r2$note, "read as item identifiers")

  # The note reaches the user beside the parse summary.
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(file = list(datapath = p1, name = "experts_only.csv"))
    html <- as.character(output$parse_note$html)
    expect_match(html, "How the first column was read")
    expect_match(html, "read as an expert")
  })
})

test_that("the logged R follows the first column branch that actually ran", {
  m <- sieve_content_demo_ratings()
  # The logged call reads the file by the name the user uploaded, so the test
  # uses the full path as that name and can then run the log from anywhere.
  cases <- list(
    numbered = data.frame(
      item = seq_len(nrow(m)),
      as.data.frame(m, check.names = FALSE),
      check.names = FALSE
    ),
    experts = as.data.frame(m, check.names = FALSE),
    named = data.frame(
      item = rownames(m),
      as.data.frame(m, check.names = FALSE),
      check.names = FALSE
    )
  )
  for (nm in names(cases)) {
    path <- tempfile(pattern = nm, fileext = ".csv")
    utils::write.csv(cases[[nm]], path, row.names = FALSE, na = "")
    st <- content_state()
    shiny::testServer(mod_content_server, args = list(state = st), {
      session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
      session$setInputs(file = list(datapath = path, name = path))
      parsed <- ratings_in()$ratings
      expect_identical(dim(parsed), c(9L, 6L))
      env <- new.env()
      eval(parse(text = paste(state$codelog, collapse = "\n")), envir = env)
      expect_identical(env$ratings, parsed)
    })
  }
})

test_that("the reproducible code log records a runnable content validity call", {
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(demo = 1)
    invisible(cvi())
    session$setInputs(keep_all_matches = 1)
    session$setInputs(record_note = 1)
    session$setInputs(keep_all_matches = 1)
    session$setInputs(ledger_tbl_rows_selected = c(7L, 9L))
    session$setInputs(park_selected = 1)
    session$setInputs(park_selected_confirm = 1)
    log <- state$codelog
    expect_true(any(grepl(
      "DMAR::content_validity_index\\(ratings, relevant = c\\(3, 4\\), essential = NULL, conf_level = 0.95\\)",
      log
    )))
    # (The exclusion is sieve_set_item_status()'s recorded mutation, one
    # item at a time with its reason.)
    expect_true(any(grepl('meta\\$status\\[meta\\$item %in% c\\("[^"]+"\\)\\] <- "parked"', log)))
    # The whole accumulated script parses as R.
    expect_no_error(parse(text = paste(log, collapse = "\n")))
    # And the logged ratings block plus the logged call actually run and
    # reproduce the table the module reported.
    env <- new.env()
    env$meta <- sieve_build_meta(
      sieve_hs9_fixture()$data,
      sieve_hs9_fixture()$item_cols
    )
    eval(parse(text = paste(log, collapse = "\n")), envir = env)
    expect_true(all(env$meta$status[env$meta$item %in%
      matched()$ledger_item[c(7L, 9L)]] == "parked"))
    ref <- DMAR::content_validity_index(sieve_content_demo_ratings(),
      relevant = c(3, 4), conf_level = 0.95
    )
    expect_identical(env$cvi$i_cvi, ref$i_cvi)
    expect_identical(env$ratings, sieve_content_demo_ratings())
  })
})

test_that("the panel computes with no data loaded, and the ledger actions gate instead of crashing", {
  # The illustrative ratings are offered only when the bundled nine test
  # example is the loaded study, so with no data loaded the ratings arrive
  # as an uploaded file.
  s <- sieve_new_state()
  path <- write_ratings_csv(sieve_content_demo_ratings(), tempfile(fileext = ".csv"))
  shiny::testServer(mod_content_server, args = list(state = s), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(file = list(datapath = path, name = "panel.csv"))
    d <- cvi()
    expect_equal(nrow(d), 9L)
    expect_match(as.character(output$ledger_gate$html), "No data loaded yet")
    # The ledger actions notice the missing ledger rather than erroring.
    expect_no_error(session$setInputs(record_note = 1))
    expect_no_error(session$setInputs(park_selected = 1))
    expect_null(state$meta)
  })
})

test_that("item names are matched on the ledger label when the data column name is not used", {
  st <- content_state()
  m <- sieve_content_demo_ratings()
  meta <- shiny::isolate(st$meta)
  rownames(m) <- meta$label # the researcher's editable Item Names
  path <- write_ratings_csv(m, tempfile(fileext = ".csv"))
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(file = list(datapath = path, name = "by_label.csv"))
    mm <- matched()
    expect_identical(mm$ledger_item, meta$item)
  })
})

# ---- Ratings read with another dataset, or before any --------------------

test_that("ratings read with one dataset say so when another is loaded, and the match follows the data now loaded", {
  txt <- function(x) paste(as.character(unlist(x)), collapse = " ")
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95, demo = 1)
    expect_identical(ratings_in()$revision$dataset_id, "D1")
    expect_false(ratings_context()$changed)
    expect_match(txt(output$parse_note), "Read with dataset D1 in force", fixed = TRUE)
    expect_match(txt(output$match_note), "9 of 9 rated items match the item ledger of dataset D1", fixed = TRUE)
    # A second dataset, loaded as the loader loads one.
    fx <- sieve_hs9_fixture()
    d2 <- fx$data[1:150, ]
    sieve_new_dataset(state)
    state$raw <- d2
    state$items <- d2[, fx$item_cols, drop = FALSE]
    state$meta <- sieve_build_meta(d2, fx$item_cols, fx$theory_map, labels = fx$labels)
    session$flushReact()
    cx <- ratings_context()
    expect_true(cx$changed)
    expect_identical(cx$loaded_with, "D1")
    expect_identical(cx$now, "D2")
    expect_match(cx$reason, "a different dataset has been loaded since", fixed = TRUE)
    note <- txt(output$parse_note)
    expect_match(note, "A different dataset has been loaded since.", fixed = TRUE)
    expect_match(note, "read with dataset D1; dataset D2 is loaded now", fixed = TRUE)
    expect_match(note, "kept as read", fixed = TRUE)
    # The ratings are not computed from the data, so they are the same
    # ratings; the match is made against the ledger now loaded.
    expect_identical(ratings_in()$ratings, sieve_content_demo_ratings())
    expect_match(txt(output$match_note), "match the item ledger of dataset D2", fixed = TRUE)
  })
  # Ratings read before any data are loaded say so, and say when data have
  # been loaded since.
  # (The ratings arrive as a file: the illustrative ratings are offered only
  # when the bundled example is the loaded study.)
  s0 <- sieve_new_state()
  p0 <- write_ratings_csv(sieve_content_demo_ratings(), tempfile(fileext = ".csv"))
  shiny::testServer(mod_content_server, args = list(state = s0), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(file = list(datapath = p0, name = "panel.csv"))
    expect_true(is.na(ratings_in()$revision$dataset_id))
    expect_match(txt(output$parse_note), "Read before any data were loaded", fixed = TRUE)
    fx <- sieve_hs9_fixture()
    sieve_new_dataset(state)
    state$raw <- fx$data
    state$items <- fx$data[, fx$item_cols, drop = FALSE]
    state$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    state$confirmed <- TRUE
    session$flushReact()
    expect_match(txt(output$parse_note), "Data have been loaded since these ratings were read", fixed = TRUE)
    expect_match(txt(output$parse_note), "read with no data loaded; dataset D1 is loaded now", fixed = TRUE)
    expect_match(txt(output$match_note), "9 of 9 rated items match the item ledger of dataset D1", fixed = TRUE)
  })
})


test_that("proposed expert matches cannot write notes until explicitly reviewed", {
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = .95, demo = 1)
    before <- state$meta
    expect_true(all(!matched()$confirmed))
    session$setInputs(record_note = 1)
    expect_identical(state$meta, before)
    session$setInputs(keep_all_matches = 1, record_note = 2)
    expect_true(all(matched()$confirmed))
    expect_true(all(grepl("I-CVI = ", state$meta$note, fixed = TRUE)))
  })
})

# ---- The content-ratings parser ------------------------------------------
# A distinct numeric first column with three or more columns is not taken
# for item identifiers by that alone, so a three by three sheet headed
# expert_1 to expert_3 keeps every expert; a blank identifier is refused,
# not answered by naming every item item_1, item_2, ... with no note; and
# the recorded read.csv call writes the file name, quotes and all, as an R
# literal.

ratings_sheet <- function(df) {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE, na = "")
  path
}

test_that("a numeric first column is read from its header and values, and what they cannot decide is asked", {
  three <- data.frame(expert_1 = c(1, 3, 4), expert_2 = c(4, 4, 3), expert_3 = c(3, 4, 4))
  r <- sieve_parse_ratings(ratings_sheet(three), "three_experts.csv")
  expect_true(r$ok)
  expect_false(r$has_names)
  expect_identical(dim(r$ratings), c(3L, 3L))
  expect_identical(colnames(r$ratings), c("expert_1", "expert_2", "expert_3"))
  expect_match(r$note, "its header names an expert", fixed = TRUE)
  # Distinct whole numbers from 1 to 4 under a header that names neither:
  # identifiers of a small pool or an expert's ratings; not guessed.
  q <- data.frame(q = c(2, 1, 4), a = c(4, 3, 3), b = c(3, 4, 4))
  path <- ratings_sheet(q)
  open <- sieve_parse_ratings(path, "q.csv")
  expect_false(open$ok)
  expect_true(open$ambiguous)
  expect_match(open$message, "could be the identifiers of a small item pool or one expert's ratings", fixed = TRUE)
  as_items <- sieve_parse_ratings(path, "q.csv", first_column = "items")
  expect_true(as_items$ok)
  expect_identical(rownames(as_items$ratings), c("2", "1", "4"))
  expect_identical(dim(as_items$ratings), c(3L, 2L))
  as_expert <- sieve_parse_ratings(path, "q.csv", first_column = "expert")
  expect_true(as_expert$ok)
  expect_identical(dim(as_expert$ratings), c(3L, 3L))
  # The page asks, and the stated reading reads the file again.
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95, first_column = "auto")
    session$setInputs(file = list(datapath = path, name = "q.csv"))
    expect_match(as.character(output$parse_note$html), "What does the first column hold?", fixed = TRUE)
    expect_false(isTRUE(ratings_in()$ok))
    session$setInputs(first_column = "items")
    expect_true(ratings_in()$ok)
    expect_identical(dim(ratings_in()$ratings), c(3L, 2L))
    expect_true(any(grepl("the first column was stated to hold the item identifiers", ratings_in()$code, fixed = TRUE)))
  })
})

test_that("a blank or repeated item identifier is refused by its rows, never replaced", {
  blank <- data.frame(item = c("x1", "", "x3", NA), a = c(4, 3, 3, 4), b = c(3, 4, 4, 2), stringsAsFactors = FALSE)
  r <- sieve_parse_ratings(ratings_sheet(blank), "blank.csv")
  expect_false(r$ok)
  expect_match(r$message, "Rows 2, 4 of blank.csv have no item identifier in the first column ('item')", fixed = TRUE)
  twice <- data.frame(item = c("x1", "x2", "x1"), a = c(4, 3, 3), b = c(3, 4, 4), stringsAsFactors = FALSE)
  r2 <- sieve_parse_ratings(ratings_sheet(twice), "twice.csv")
  expect_false(r2$ok)
  expect_match(r2$message, "names an item more than once: x1", fixed = TRUE)
  # Without an item column the rows are numbered.
  m <- sieve_content_demo_ratings()
  r3 <- sieve_parse_ratings(ratings_sheet(as.data.frame(m, check.names = FALSE)), "experts_only.csv")
  expect_true(r3$ok)
  expect_identical(r3$item_source, "numbered")
})

test_that("the recorded read.csv call quotes the file name as an R literal", {
  m <- sieve_content_demo_ratings()
  path <- ratings_sheet(data.frame(item = rownames(m), as.data.frame(m, check.names = FALSE), check.names = FALSE))
  st <- content_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95, first_column = "auto")
    session$setInputs(file = list(datapath = path, name = 'panel "A" ratings.csv'))
    code <- ratings_in()$code
    expect_identical(code[[1L]], 'ratings <- utils::read.csv("panel \\"A\\" ratings.csv", stringsAsFactors = FALSE, check.names = FALSE)')
    expect_no_error(parse(text = code))
  })
})

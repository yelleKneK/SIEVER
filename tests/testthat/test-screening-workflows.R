# The Screening tab's workflows: the Item-total panel that computes whatever
# the constructs are and excludes the items selected in it, the
# careless-responding screens explained at the point of use with respondents
# removed from (and restored to) the analysis with a reason, and the
# correlations printed in the heatmap's cells, looked up pair by pair, and
# shown as a table.
#
# Author: Ken Kelley

skip_if_not_installed("DMAR")
skip_if_not_installed("shiny")

# A loaded, confirmed state on the primary nine tests (n = 301; columns id,
# sex, grade, age_years, school are not items; id is the identifier).
ns_hs9_state <- function() {
  siever_test_study(configure = function(s) s$roles <- NULL)
}

ns_text <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(unlist(x)), collapse = " ")
}

# The corrected item total correlation by hand: the item against the mean
# of the other items of its group, over the respondents with both.
ns_itc_by_hand <- function(d, group) {
  vapply(seq_along(d), function(i) {
    peers <- setdiff(which(group == group[i]), i)
    if (!length(peers)) return(NA_real_)
    rest <- rowMeans(as.matrix(d[, peers, drop = FALSE]), na.rm = TRUE)
    stats::cor(d[[i]], rest, use = "complete.obs")
  }, numeric(1))
}

# ---- The Item-total panel ----------------------------------------------------

test_that("items without a construct are totaled together and the panel says what it computed", {
  st <- ns_hs9_state()
  # Two items without a construct (NA, as a saved project or a hand-built
  # ledger may have it; "" and "Unassigned" are read the same way). An NA must
  # not stop the whole table with "undefined columns selected".
  shiny::isolate({
    m <- st$meta
    m$construct[m$item == "t1_visual_perception"] <- NA_character_
    m$construct[m$item == "t2_cubes"] <- "Unassigned"
    st$meta <- m
  })
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_screen_server, args = list(state = st), {
    df <- itc()
    expect_identical(nrow(df), 9L)
    expect_identical(
      df$construct_shown[df$item %in% c("t1_visual_perception", "t2_cubes")],
      rep("(no construct)", 2L)
    )
    # Every correlation against an independent computation by hand: the two
    # items without a construct with each other, the rest within construct.
    d <- as.data.frame(lapply(shiny::isolate(state$items)[, df$item], as.numeric))
    group <- ifelse(df$unassigned, "(none)", df$construct)
    expect_equal(df$item_total, ns_itc_by_hand(d, group), tolerance = 1e-12)
    expect_equal(df$item_total[df$item == "t1_visual_perception"],
      stats::cor(d$t1_visual_perception, d$t2_cubes),
      tolerance = 1e-12
    )
    # Visual keeps one item (t4_lozenges), which has no peer.
    expect_identical(df$n[df$item != "t4_lozenges"], rep(301L, 8L))
    expect_identical(df$note[df$item == "t4_lozenges"], "the only included item in its construct")
    note <- ns_text(output$itc_note)
    expect_match(note, "9 included items.", fixed = TRUE)
    expect_match(note, "1 correlation could not be computed (the only included item in its construct: 1)",
      fixed = TRUE
    )
    expect_match(note, "Totals are formed within 3 constructs (Visual, Textual, Speed)", fixed = TRUE)
    expect_match(note, "the 2 items with no construct are totaled together", fixed = TRUE)
    expect_match(note, "Computed on the 301 respondents in the analysis", fixed = TRUE)
    expect_no_error(output$itc_tbl)
  })
})

test_that("with no construct anywhere every item is totaled against all the others", {
  st <- ns_hs9_state()
  shiny::isolate({
    m <- st$meta
    m$construct <- NA_character_
    st$meta <- m
  })
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_screen_server, args = list(state = st), {
    df <- itc()
    d <- as.data.frame(lapply(shiny::isolate(state$items)[, df$item], as.numeric))
    expect_equal(df$item_total, ns_itc_by_hand(d, rep("all", 9L)), tolerance = 1e-12)
    expect_identical(df$n_peers, rep(8L, 9L))
    expect_match(ns_text(output$itc_note),
      "No construct is assigned, so each item is correlated with the mean of all other included items.",
      fixed = TRUE
    )
  })
})

test_that("a correlation that cannot be computed is named with its reason, never a silent blank", {
  st <- ns_hs9_state()
  shiny::isolate({
    m <- st$meta
    m$construct[m$item == "t1_visual_perception"] <- "Alone"
    st$meta <- m
    it <- st$items
    it$t9_word_meaning <- 4
    st$items <- it
  })
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_screen_server, args = list(state = st), {
    df <- itc()
    expect_true(is.na(df$item_total[df$item == "t1_visual_perception"]))
    expect_identical(df$note[df$item == "t1_visual_perception"], "the only included item in its construct")
    expect_true(is.na(df$item_total[df$item == "t9_word_meaning"]))
    expect_identical(df$note[df$item == "t9_word_meaning"], "no variance: every respondent gave the same answer")
    note <- ns_text(output$itc_note)
    expect_match(note, "2 correlations could not be computed", fixed = TRUE)
    expect_match(note, "the only included item in its construct: 1", fixed = TRUE)
    tbl <- ns_text(output$itc_tbl)
    expect_match(tbl, "Note", fixed = TRUE)
  })
  # Before the study is confirmed the panel says why it is empty.
  st2 <- ns_hs9_state()
  shiny::isolate(st2$confirmed <- FALSE)
  shiny::testServer(mod_screen_server, args = list(state = st2), {
    expect_match(ns_text(output$itc_note), "until the data are loaded and the study is confirmed", fixed = TRUE)
  })
})

test_that("the item total table is computed while its panel is hidden and carries its rows", {
  src <- deparse(body(mod_screen_server))
  expect_true(any(grepl("outputOptions(output, \"itc_tbl\", suspendWhenHidden = FALSE)", src, fixed = TRUE)))
  # The rows travel with the table (server = FALSE), so a browser draws them
  # while the session computes something long (the heatmap's polychoric
  # matrix takes about 50 s on a study of 500 respondents by 68 ordered
  # items).
  st <- ns_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    payload <- jsonlite::fromJSON(output$itc_tbl, simplifyVector = FALSE)
    expect_length(payload$x$data[[1]], 9L)
    expect_identical(unlist(payload$x$data[[1]]), itc()$item)
  })
})

test_that("sieve_exclude_items sets the status, the reason and the log, as the Use cell does", {
  st <- ns_hs9_state()
  out <- sieve_exclude_items(st, c("t2_cubes", "t4_lozenges", "not_an_item"),
    reason = "Weak item total correlation\nand a second line"
  )
  expect_identical(out$changed, c("t2_cubes", "t4_lozenges"))
  expect_identical(out$unchanged, "not_an_item")
  m <- shiny::isolate(st$meta)
  expect_identical(m$status[m$item %in% c("t2_cubes", "t4_lozenges")], c("parked", "parked"))
  expect_identical(m$exclude_reason[m$item == "t2_cubes"], "Weak item total correlation\nand a second line")
  expect_true(all(is.na(m$exclude_reason[m$status == "active"])))
  log <- shiny::isolate(st$codelog)
  expect_true(any(grepl("Drop 2 items from further consideration: t2_cubes, t4_lozenges (reason: ", log, fixed = TRUE)))
  expect_true("meta$status[meta$item %in% c(\"t2_cubes\", \"t4_lozenges\")] <- \"parked\"" %in% log)
  # The reason is comment lines, whatever it holds: no line of it is code.
  reason_lines <- log[grepl("Weak item total|a second line", log)]
  expect_no_error(parse(text = log))
  expect_true(any(grepl("exclude_reason", reason_lines, fixed = TRUE)))
  # Again: nothing changes, nothing is logged; no reason gives NA.
  n_log <- length(log)
  again <- sieve_exclude_items(st, "t2_cubes")
  expect_identical(again$changed, character(0))
  expect_identical(length(shiny::isolate(st$codelog)), n_log)
  sieve_exclude_items(st, "t6_paragraph_comprehension")
  m <- shiny::isolate(st$meta)
  expect_true(is.na(m$exclude_reason[m$item == "t6_paragraph_comprehension"]))
  expect_identical(m$status[m$item == "t6_paragraph_comprehension"], "parked")
})

test_that("the selected rows of the Item-total table are excluded with the reason, and two items stay", {
  st <- ns_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    df <- itc()
    picked <- df$item[c(2, 5)]
    session$setInputs(itc_tbl_rows_selected = c(2L, 5L), itc_reason = "Low corrected item total r")
    session$setInputs(itc_exclude = 1L)
    m <- state$meta
    expect_identical(m$status[match(picked, m$item)], c("parked", "parked"))
    expect_identical(m$exclude_reason[match(picked, m$item)], rep("Low corrected item total r", 2L))
    expect_match(ns_text(output$itc_result), "Excluded 2 items from every analysis", fixed = TRUE)
    # Every view follows: the table now has seven items.
    expect_identical(nrow(itc()), 7L)
    expect_false(any(picked %in% included_items()))
    # Nothing selected: nothing happens, and the panel says what to do.
    session$setInputs(itc_tbl_rows_selected = NULL, itc_exclude = 2L)
    expect_match(ns_text(output$itc_result), "Select one or more rows", fixed = TRUE)
    # At least two items stay included.
    session$setInputs(itc_tbl_rows_selected = 1:6, itc_exclude = 3L)
    expect_identical(sum(state$meta$status == "active"), 7L)
    expect_match(ns_text(output$itc_result), "at least two must stay included", fixed = TRUE)
  })
})

# ---- Careless responding explained, respondents removed and restored ---------

test_that("respondents are named by their row of the data as loaded and their identifier", {
  st <- ns_hs9_state()
  ids <- shiny::isolate(sieve_case_ids(st))
  expect_identical(attr(ids, "variable"), "id")
  expect_identical(ids$row, 1:301)
  expect_identical(ids$id, as.character(shiny::isolate(st$raw$id)))
  # case_index stays NULL until a respondent is removed.
  expect_null(shiny::isolate(st$case_index))
  # Typed entries: identifiers first, then rows; repeats once; the rest named.
  found <- sieve_case_parse("11, 349; 3 11\nnope", ids)
  expect_identical(found$rows, c(10L, 300L, 3L))
  expect_identical(found$unmatched, "nope")
  # Without an identifier column every entry is a row.
  shiny::isolate({
    roles <- sieve_roles_table(st$raw, st$items, st$roles)
    roles$role[roles$variable == "id"] <- "auxiliary"
    st$roles <- roles
  })
  no_id <- shiny::isolate(sieve_case_ids(st))
  expect_true(is.na(attr(no_id, "variable")))
  expect_identical(sieve_case_parse("11 349", no_id)$rows, c(11L, NA)[1])
  expect_identical(sieve_case_parse("11 349", no_id)$unmatched, "349")
})

test_that("removal keeps the data as loaded and the item data aligned, and restore puts every row back exactly", {
  st <- ns_hs9_state()
  raw0 <- shiny::isolate(st$raw)
  items0 <- shiny::isolate(st$items)
  meta0 <- shiny::isolate(st$meta)
  out <- sieve_remove_cases(st, c(300, 3, 10, 999), "Straight-lining on every page",
    flagged_by = c("300" = "Mahalanobis", "3" = "Longstring; Within-person SD", "10" = "")
  )
  expect_identical(out$removed, c(3L, 10L, 300L))
  expect_identical(out$unknown, 999L)
  shiny::isolate({
    keep <- setdiff(1:301, c(3L, 10L, 300L))
    expect_identical(st$case_index, keep)
    expect_identical(nrow(st$raw), 298L)
    expect_identical(nrow(st$items), 298L)
    # The same respondents leave both, in step.
    expect_identical(st$raw$id, raw0$id[keep])
    expect_equal(as.data.frame(st$items), as.data.frame(items0[keep, , drop = FALSE]), ignore_attr = TRUE)
    expect_identical(sieve_data_key(st$items), sieve_data_key(items0[keep, , drop = FALSE]))
    # The ledger describes the remaining rows: against an independent
    # computation on the reduced data.
    m <- st$meta
    expect_equal(m$mean, unname(colMeans(items0[keep, m$item])), tolerance = 1e-12)
    expect_equal(m$sd, unname(apply(items0[keep, m$item], 2, stats::sd)), tolerance = 1e-12)
    expect_equal(m$min, unname(apply(items0[keep, m$item], 2, min)))
    expect_equal(m$max, unname(apply(items0[keep, m$item], 2, max)))
    # The held respondents, in the order of the data as loaded.
    tbl <- sieve_removed_table(st)
    expect_identical(tbl$row, c(3L, 10L, 300L))
    expect_identical(tbl$id, as.character(raw0$id[c(3, 10, 300)]))
    expect_identical(tbl$flagged_by, c("Longstring; Within-person SD", "", "Mahalanobis"))
    expect_identical(tbl$reason, rep("Straight-lining on every page", 3L))
    # The step and its R, with the reason as a comment.
    log <- st$codelog
    # The session script names the respondents by their rows of the data as
    # loaded, never by an identifier.
    expect_true(any(grepl("Screening: remove 3 respondents from the analysis: rows 3, 10, 300 of the data as loaded (reason: Straight-lining on every page); 298 remain",
      log,
      fixed = TRUE
    )))
    expect_true("# Reason:     Straight-lining on every page" %in% log)
    expect_true(any(grepl("out <- case_index %in% c(3L, 10L, 300L)", log, fixed = TRUE)))
  })
  # Restore one, then the rest: every row returns to its original position.
  back <- sieve_restore_cases(st, c(10, 5))
  expect_identical(back$restored, 10L)
  expect_identical(back$unknown, 5L)
  shiny::isolate({
    keep <- setdiff(1:301, c(3L, 300L))
    expect_identical(st$case_index, keep)
    expect_identical(st$raw$id, raw0$id[keep])
    expect_identical(sieve_removed_table(st)$row, c(3L, 300L))
  })
  sieve_restore_cases(st, c(3, 300))
  shiny::isolate({
    expect_null(st$case_index)
    expect_identical(st$removed_cases, list())
    expect_identical(st$raw, raw0)
    expect_identical(st$items, items0)
    expect_equal(st$meta[c("mean", "sd", "min", "max", "n_missing", "n_levels")],
      meta0[c("mean", "sd", "min", "max", "n_missing", "n_levels")],
      tolerance = 1e-12
    )
    expect_true(any(grepl("Screening: restore 2 respondents to the analysis: rows 3, 300 of the data as loaded; 301 in the analysis",
      st$codelog,
      fixed = TRUE
    )))
  })
  # Fewer than three respondents may not remain.
  refused <- sieve_remove_cases(st, 1:299, "too many")
  expect_identical(refused$removed, integer(0))
  expect_match(refused$refused, "would leave 2 in the analysis", fixed = TRUE)
  expect_identical(nrow(shiny::isolate(st$raw)), 301L)
})

test_that("an item reverse scored after a removal reaches the restored rows exactly", {
  st <- ns_hs9_state()
  items0 <- shiny::isolate(st$items)
  sieve_remove_cases(st, c(2, 150), "test")
  rev <- sieve_apply_reverse(st, "t2_cubes", lo = 0, hi = 40)
  expect_identical(names(rev$applied), "t2_cubes")
  sieve_restore_cases(st, 150)
  sieve_apply_reverse(st, "t4_lozenges", lo = 0, hi = 40)
  sieve_restore_cases(st, 2)
  shiny::isolate({
    expected <- items0
    expected$t2_cubes <- 40 - as.numeric(items0$t2_cubes)
    expected$t4_lozenges <- 40 - as.numeric(items0$t4_lozenges)
    expect_identical(st$items, expected)
    # Undo the first reversal: every row is again at its values as loaded.
    sieve_apply_reverse(st, "t2_cubes")
    expect_identical(st$items$t2_cubes, items0$t2_cubes)
  })
})

test_that("labelled, factor and date columns keep their storage through a removal and a restore", {
  st <- ns_hs9_state()
  shiny::isolate({
    raw <- st$raw
    raw$id <- structure(as.numeric(raw$id),
      class = c("haven_labelled", "vctrs_vctr", "double"),
      label = "Respondent", labels = c(first = 1)
    )
    raw$school <- factor(raw$school)
    raw$when <- as.Date("2026-01-01") + seq_len(nrow(raw))
    it <- st$items
    it$t1_visual_perception <- structure(as.numeric(it$t1_visual_perception),
      class = c("haven_labelled", "vctrs_vctr", "double"),
      label = "Visual perception"
    )
    raw$t1_visual_perception <- it$t1_visual_perception
    st$raw <- raw
    st$items <- it
    st$roles <- NULL
  })
  raw0 <- shiny::isolate(st$raw)
  items0 <- shiny::isolate(st$items)
  sieve_remove_cases(st, c(1, 7, 301), "storage")
  shiny::isolate({
    expect_s3_class(st$raw$id, "haven_labelled")
    expect_identical(attr(st$raw$id, "labels"), c(first = 1))
    expect_s3_class(st$raw$school, "factor")
    expect_identical(levels(st$raw$school), levels(raw0$school))
    expect_s3_class(st$raw$when, "Date")
    expect_identical(sieve_removed_table(st)$id, sieve_case_id_text(raw0$id[c(1, 7, 301)]))
    expect_identical(sieve_removed_table(st)$id, c("1", "7", "351"))
  })
  sieve_restore_cases(st, c(301, 1, 7))
  expect_identical(shiny::isolate(st$raw), raw0)
  expect_identical(shiny::isolate(st$items), items0)
})

test_that("a new dataset clears the removal, and a project file keeps it", {
  st <- ns_hs9_state()
  shiny::isolate(st$estimator <- "ML")
  sieve_remove_cases(st, c(4, 40), "Kept in the project")
  snap <- sieve_project_snapshot(st)
  f <- withr::local_tempfile(fileext = ".siever.rds")
  sieve_project_write(snap, f)
  opened <- sieve_new_state()
  sieve_project_restore(opened, sieve_project_read(f))
  shiny::isolate({
    expect_identical(opened$case_index, shiny::isolate(st$case_index))
    expect_identical(sieve_removed_table(opened), shiny::isolate(sieve_removed_table(st)))
    expect_identical(nrow(opened$raw), 299L)
  })
  # The opened project restores the respondents exactly.
  sieve_restore_cases(opened, c(4, 40))
  expect_identical(shiny::isolate(opened$raw), sieve_hs9_fixture()$data)
  # A new load starts with every respondent.
  shiny::isolate(sieve_new_dataset(st))
  expect_null(shiny::isolate(st$case_index))
  expect_identical(shiny::isolate(st$removed_cases), list())
})

test_that("the careless panel defines each screen with its numbers, names the screens, and removes and restores", {
  st <- ns_hs9_state()
  # Two straight-liners, so the screens flag someone.
  shiny::isolate({
    it <- st$items
    it[5, ] <- 20
    it[9, ] <- 20
    st$items <- it
    raw <- st$raw
    raw[5, names(it)] <- 20
    raw[9, names(it)] <- 20
    st$raw <- raw
  })
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_screen_server, args = list(state = st), {
    r <- careless()
    expect_identical(r$tbl$row, 1:301)
    expect_identical(r$tbl$id, as.character(shiny::isolate(state$raw$id)))
    expect_identical(r$tbl$flagged_by[5], "Longstring; Within-person SD; Mahalanobis")
    defs <- ns_text(output$careless_note)
    expect_match(defs, "Straightlining review: longstring", fixed = TRUE)
    expect_match(defs, "9 selected items in the data file's column order, before reverse scoring", fixed = TRUE)
    expect_match(defs, "Flag cutoff: at least 5 consecutive equal answers", fixed = TRUE)
    expect_match(defs, "Within-person response variability", fixed = TRUE)
    expect_match(defs, "D2 = (x - xbar)' S^-1 (x - xbar)", fixed = TRUE)
    expect_match(defs, "Evaluated for 301 of 301 observations (301 complete)", fixed = TRUE)
    expect_match(defs, "chi-square tail on 9 df", fixed = TRUE)
    # Respondents are removed and restored, in the words of the respondent
    # tools.
    expect_match(defs, "A flag never removes a respondent by itself", fixed = TRUE)
    tbl <- ns_text(output$careless_tbl)
    expect_match(tbl, "Screens flagged (of 3)", fixed = TRUE)
    expect_match(tbl, "Flagged by", fixed = TRUE)
    expect_match(tbl, "Identifier (id)", fixed = TRUE)
    expect_false(grepl("Screens tripped", tbl, fixed = TRUE))
    # Remove the first flagged row (selected) and a typed identifier.
    flagged <- careless_flagged()
    first <- flagged$row[1]
    session$setInputs(
      careless_tbl_rows_selected = 1L, remove_ids = "11",
      remove_reason = "Straight-lining; also id 11 by protocol"
    )
    session$setInputs(remove_go = 1L)
    expect_identical(
      sort(state$removed_cases %||% list() |> vapply(function(x) x$row, integer(1))),
      sort(c(first, 10L))
    )
    expect_identical(nrow(state$raw), 299L)
    expect_match(ns_text(output$remove_result), "Removed 2 respondents from the analysis", fixed = TRUE)
    expect_match(ns_text(output$removed_note), "2 respondents are removed from the analysis; 299 remain", fixed = TRUE)
    expect_match(ns_text(output$pool_note), "299 respondents are in the analysis; 2 are removed from it", fixed = TRUE)
    rt <- removed_shown()
    expect_identical(rt$flagged_by[rt$row == 10L], "not flagged")
    expect_identical(rt$flagged_by[rt$row == first], flagged$flagged_by[1])
    # The screens are computed on the rows that remain, and keep their rows.
    expect_false(any(careless()$tbl$row %in% c(first, 10L)))
    expect_identical(nrow(careless()$tbl), 299L)
    # Restore the row 10 respondent.
    session$setInputs(removed_tbl_rows_selected = which(rt$row == 10L), restore_go = 1L)
    expect_identical(nrow(state$raw), 300L)
    expect_identical(sieve_removed_table(state)$row, first)
    expect_match(ns_text(output$restore_result),
      "Restored 1 respondent to the analysis in their original rows",
      fixed = TRUE
    )
    # Nothing selected or typed: nothing happens and the panel says so.
    session$setInputs(careless_tbl_rows_selected = NULL, remove_ids = "", remove_go = 2L)
    expect_match(ns_text(output$remove_result), "nothing was removed", fixed = TRUE)
  })
  html <- paste(as.character(mod_screen_ui("screen")), collapse = "")
  expect_false(grepl("SIEVER does not remove cases", html, fixed = TRUE))
  expect_match(html, "Remove from the analysis", fixed = TRUE)
  expect_match(html, "Restore the selected", fixed = TRUE)
})

test_that("the identifier column shown follows the Variable roles", {
  st <- ns_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    expect_identical(careless()$id_variable, "id")
    expect_match(ns_text(output$remove_hint), "values of the identifier column id", fixed = TRUE)
    roles <- sieve_roles_table(state$raw, state$items, state$roles)
    roles$role[roles$variable == "id"] <- "auxiliary"
    state$roles <- roles
    sieve_confirm_types(state)
    session$flushReact()
    expect_true(is.na(careless()$id_variable))
    expect_true(all(is.na(careless()$tbl$id)))
    expect_match(ns_text(output$remove_hint), "These data have no identifier column", fixed = TRUE)
  })
})

test_that("the recorded removal and restore replay on the views of the item data, by row when there is no identifier", {
  st <- ns_hs9_state()
  shiny::isolate({
    roles <- sieve_roles_table(st$raw, st$items, st$roles)
    roles$role[roles$variable == "id"] <- "auxiliary"
    st$roles <- roles
  })
  sieve_remove_cases(st, c(6, 2), "rows")
  sieve_apply_reverse(st, "t2_cubes", lo = 0, hi = 40)
  sieve_remove_cases(st, 100, "another")
  sieve_restore_cases(st, c(2, 100))
  log <- shiny::isolate(st$codelog)
  expect_true(any(grepl("out <- case_index %in% c(2L, 6L)", log, fixed = TRUE)))
  # Run the recorded steps on the views the session script builds.
  fx <- sieve_hs9_fixture()
  env <- new.env(parent = globalenv())
  env$d <- fx$data
  env$items <- fx$data[, fx$item_cols, drop = FALSE]
  env$active_items <- env$items
  env$X <- as.matrix(env$items)
  stamps <- which(startsWith(log, "# ["))
  ends <- c(stamps[-1L] - 1L, length(log))
  for (k in seq_along(stamps)) {
    eval(parse(text = log[stamps[k]:ends[k]]), envir = env)
  }
  shiny::isolate({
    expect_identical(env$case_index, st$case_index)
    expect_identical(env$d$id, st$raw$id)
    expect_equal(as.data.frame(env$items), as.data.frame(st$items), ignore_attr = TRUE)
    # The matrix X of the careless step is not reached by a recorded
    # reversal (it has no names), and its restored rows are not either.
    expect_equal(unname(env$X), unname(as.matrix(fx$data[st$case_index, fx$item_cols])))
    expect_equal(as.data.frame(env$active_items), as.data.frame(st$items), ignore_attr = TRUE)
  })
})

test_that("the session script with a removal, a reversal and a restore replays in a clean R process", {
  skip_on_cran()
  root <- compat_source_root()
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    suppressMessages(session$setInputs(load = 1L))
  })
  expect_true(shiny::isolate(st$confirmed))
  shiny::testServer(mod_screen_server, args = list(state = st), {
    invisible(careless())
    session$setInputs(remove_ids = "11 349 3", remove_reason = "Replay check")
    session$setInputs(remove_go = 1L)
  })
  sieve_apply_reverse(st, "t2_cubes", lo = 0, hi = 40)
  sieve_restore_cases(st, 300)
  got <- new.env(parent = emptyenv())
  shiny::testServer(mod_repro_server, args = list(state = st), {
    got$script <- script()
  })
  dir <- withr::local_tempdir()
  writeLines(got$script, file.path(dir, "siever_session.R"))
  runner <- file.path(dir, "runner.R")
  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
    if (is.null(root)) "library(SIEVER)" else
      sprintf("pkgload::load_all(%s, quiet = TRUE, export_all = FALSE, helpers = FALSE)", deparse(root)),
    sprintf("setwd(%s)", deparse(dir)),
    "lines <- readLines('siever_session.R')",
    "stamps <- which(startsWith(lines, '# ['))",
    "starts <- c(1L, stamps); ends <- c(stamps - 1L, length(lines))",
    "for (i in seq_along(starts)) {",
    "  if (ends[i] < starts[i]) next",
    "  label <- if (i == 1L) '(header)' else lines[starts[i]]",
    "  ok <- tryCatch({ eval(parse(text = lines[starts[i]:ends[i]]), envir = globalenv()); TRUE },",
    "                 error = function(e) { cat('STEP_FAILED ', label, ' :: ', conditionMessage(e), '\\n', sep = ''); FALSE })",
    "  if (!ok) quit(status = 1)",
    "}",
    "saveRDS(list(d = d, items = items, case_index = case_index, X = X), 'replayed.rds')"
  ), runner)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner)),
    stdout = TRUE, stderr = TRUE
  ))
  expect_identical(grep("^STEP_FAILED", out, value = TRUE), character(0))
  replayed <- readRDS(file.path(dir, "replayed.rds"))
  shiny::isolate({
    expect_identical(replayed$case_index, st$case_index)
    expect_identical(sieve_data_key(replayed$d), sieve_data_key(st$raw))
    expect_identical(sieve_data_key(replayed$items[, names(st$items)]), sieve_data_key(st$items))
    expect_identical(nrow(replayed$X), nrow(st$items))
  })
})
# ---- The correlations themselves, on the heatmap and beside it ---------------

test_that("sieve_correlation_heatmap prints the values only when asked, with the text color chosen for contrast", {
  R <- stats::cor(attitude)
  R[1, 2] <- R[2, 1] <- 0.95
  R[1, 3] <- R[3, 1] <- -0.95
  R[1, 4] <- R[4, 1] <- 0
  R[1, 5] <- R[5, 1] <- -0.001
  f <- withr::local_tempfile(fileext = ".pdf")
  size <- sieve_screen_heatmap_size(7, values = TRUE)
  grDevices::pdf(f, width = size$width_in, height = size$height_in)
  plain <- sieve_correlation_heatmap(R)
  shown <- sieve_correlation_heatmap(R, values = TRUE)
  auto <- sieve_correlation_heatmap(R, values = NULL)
  grDevices::dev.off()
  # FALSE by default; TRUE prints every tile to two decimals, the leading
  # zero dropped; NULL keeps the rule for 16 items or fewer.
  expect_false(plain$values)
  expect_null(plain$text)
  expect_true(shown$values)
  expect_identical(shown$text[1, 2], ".95")
  expect_identical(shown$text[1, 3], "-.95")
  expect_identical(shown$text[1, 1], "1.00")
  expect_identical(shown$text[1, 5], ".00")
  expect_identical(shown$text[2, 5], sub("^(-?)0\\.", "\\1.", sprintf("%.2f", R[2, 5])))
  expect_true(auto$values)
  # The ink: white on the dark ends of the scale, the app's dark text on the
  # white middle, by the WCAG contrast ratio computed here independently.
  ink <- attr(shown$text, "color")
  expect_identical(ink[1, 2], "#ffffff")
  expect_identical(ink[1, 3], "#ffffff")
  expect_identical(ink[1, 4], "#1e1913")
  rel_lum <- function(hex) {
    v <- grDevices::col2rgb(hex)[, 1] / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  navy <- sieve_palette_colors("nd", 1L)[1]
  expect_gt((1.05) / (rel_lum(navy) + 0.05), (rel_lum(navy) + 0.05) / (rel_lum("#1e1913") + 0.05))
  expect_identical(sieve_screen_text_color(c(navy, "#ffffff", "#e4e4e4")), c("#ffffff", "#1e1913", "#1e1913"))
  # Tiles of 28 px when the values are printed, 14 px at least otherwise.
  expect_equal(sieve_screen_heatmap_size(68, values = TRUE)$cell, 28)
  expect_equal(sieve_screen_heatmap_size(68)$cell, 14)
  expect_equal(sieve_screen_heatmap_size(9, values = TRUE)$cell, 40)
})

test_that("the checkbox prints the values on screen, in the downloads and in the recorded R", {
  st <- ns_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    # Nine items: printed unless the checkbox says otherwise.
    expect_true(heat_args()$values)
    session$setInputs(cor_values = FALSE)
    expect_false(heat_args()$values)
    session$setInputs(cor_values = TRUE)
    expect_true(heat_args()$values)
    png <- output$dl_png
    log <- shiny::isolate(state$codelog)
    code <- sieve_screen_heatmap_code(heat_args(), heat_size(), "siever_correlations_pearson.png", "png")
    expect_true(all(code %in% log))
    expect_true(any(grepl("values = TRUE)", code, fixed = TRUE)))
    expect_true(any(grepl("the correlations printed in the cells", code, fixed = TRUE)))
    # The recorded R writes the same figure, byte for byte.
    env <- new.env(parent = globalenv())
    assign("R_items", cor_mat()$R, envir = env)
    assign("meta", shiny::isolate(state$meta), envir = env)
    withr::with_dir(withr::local_tempdir(), {
      eval(parse(text = code), envir = env)
      expect_identical(readBin("siever_correlations_pearson.png", "raw", 1e7), readBin(png, "raw", 1e7))
    })
    session$setInputs(cor_values = FALSE)
    code_off <- sieve_screen_heatmap_code(heat_args(), heat_size(), "x.png", "png")
    expect_true(any(grepl("values = FALSE)", code_off, fixed = TRUE)))
  })
  expect_null(grDevices::dev.list())
})

test_that("a pool of more than 30 items prints the values in larger tiles and says to choose a block", {
  withr::local_seed(20261015)
  f <- matrix(stats::rnorm(200 * 2), 200, 2)
  X <- as.data.frame(sapply(1:40, function(j) 0.7 * f[, 1 + (j > 20)] + stats::rnorm(200)))
  names(X) <- sprintf("v%02d", 1:40)
  st <- sieve_new_state()
  shiny::isolate({
    sieve_new_dataset(st)
    st$raw <- X
    st$items <- X
    map <- list(First = names(X)[1:20], Second = names(X)[21:40])
    st$meta <- sieve_build_meta(X, names(X), map)
    st$confirmed <- TRUE
  })
  shiny::testServer(mod_screen_server, args = list(state = st), {
    expect_false(heat_values())
    session$setInputs(cor_values = TRUE)
    expect_equal(heat_size()$cell, 28)
    note <- ns_text(output$cor_scope_note)
    expect_match(note, "drawn at 28 px for them, so the figure of 40 items is large and scrolls", fixed = TRUE)
    expect_match(note, "Choose one construct or facet with Show", fixed = TRUE)
    session$setInputs(cor_scope = "construct:First")
    expect_match(ns_text(output$cor_scope_note), "The correlations are printed in the cells, to two decimals.",
      fixed = TRUE
    )
  })
})

test_that("one pair is looked up by item name with the kind of correlation and n", {
  st <- ns_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    session$setInputs(pair_a = "t2_cubes", pair_b = "t9_word_meaning")
    p <- pair()
    d <- shiny::isolate(state$items)
    expect_equal(p$r, stats::cor(d$t2_cubes, d$t9_word_meaning), tolerance = 1e-12)
    expect_identical(p$kind, "pearson")
    expect_identical(as.integer(p$n), 301L)
    txt <- ns_text(output$pair_r)
    expect_match(txt, sprintf("r = %s", sub("^(-?)0\\.", "\\1.", sprintf("%.3f", p$r))), fixed = TRUE)
    expect_match(txt, "Pearson correlation of", fixed = TRUE)
    expect_match(txt, "computed on the observed item scores; n = 301 respondents", fixed = TRUE)
    lab <- shiny::isolate(state$meta$label[state$meta$item == "t2_cubes"])
    expect_match(txt, lab, fixed = TRUE)
  })
  # A polychoric pair names its kind.
  cm <- list(
    R = matrix(c(1, 0.4, 0.4, 1), 2, dimnames = list(c("a", "b"), c("a", "b"))),
    kind = "polychoric", n = 480, n_all = 500
  )
  info <- data.frame(item = c("a", "b"), label = c("Item A", "Item B"), stringsAsFactors = FALSE)
  out <- sieve_screen_pair(cm, "a", "b", info)
  expect_identical(out$text, paste0(
    "r = .400: the polychoric correlation of Item A with Item B, estimated from the ",
    "ordered responses (psych); n = 480 respondents who answered every included item ",
    "(complete cases, of 500)."
  ))
})

test_that("the correlation table is the matrix as shown, scope applied, with item names", {
  st <- ns_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    df <- cor_table_df()
    R <- cor_mat()$R
    expect_identical(names(df), c("Item", rownames(R)))
    expect_identical(df$Item, item_info()$label)
    expect_equal(unname(as.matrix(df[, -1])), unname(R), tolerance = 0)
    tbl <- ns_text(output$cor_table)
    expect_match(tbl, sieve_screen_short_label(item_info()$label[1]), fixed = TRUE)
    session$setInputs(cor_scope = "construct:Speed")
    speed <- item_info()$item[item_info()$construct == "Speed"]
    expect_identical(names(cor_table_df())[-1], speed)
    expect_equal(unname(as.matrix(cor_table_df()[, -1])), unname(R[speed, speed]), tolerance = 0)
  })
  html <- paste(as.character(mod_screen_ui("screen")), collapse = "")
  expect_match(html, "Show the correlations in the cells", fixed = TRUE)
  expect_match(html, "Correlation table", fixed = TRUE)
  expect_match(html, "with item", fixed = TRUE)
})

# ---- Identifiers, flagged responses and the sampling design --------------------

sw_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", ns_text(x)))

# With an identifier column, an entry that matches no identifier is not
# read as a row number, since a mistyped identifier that happened to be a
# valid row would remove a different respondent in silence. A flagged
# respondent's responses are read where the flag is. The study's sampling
# design is declared once, study-wide, not in the SEM tab's sidebar.

test_that("an identifier typo is never read as a row: rows are named with #, and the resolution is shown before the removal", {
  st <- ns_hs9_state()
  ids <- shiny::isolate(sieve_case_ids(st))
  # A row number that is no identifier here (the identifiers skip some
  # numbers), as a mistyped identifier would be.
  # Row 10 is id 11, named below, so the typo is another row.
  typo <- setdiff(as.character(seq_len(nrow(ids))), c(ids$id, "10"))[[1L]]
  row <- as.integer(typo)
  parsed <- sieve_case_parse(typo, ids)
  expect_identical(parsed$rows, integer(0))
  expect_identical(parsed$unmatched, typo)
  expect_match(parsed$hint, sprintf(
    "%s is not an identifier of id; to name a row of the data as loaded, type it with a leading # (for example #%s).", typo, typo
  ), fixed = TRUE)
  explicit <- sieve_case_parse(paste0("#", typo, " 11"), ids)
  expect_identical(explicit$rows, c(row, 10L))
  expect_identical(explicit$resolved$how, c("row", "identifier"))
  expect_identical(
    sieve_case_parse_text(explicit, "id"),
    sprintf("Remove would take out: id %s (row %d), id 11 (row 10).", ids$id[row], row)
  )
  shiny::testServer(mod_screen_server, args = list(state = st), {
    session$setInputs(remove_ids = paste(typo, "11"), remove_reason = "a typo test")
    preview <- sw_text(output$remove_preview)
    expect_match(preview, sprintf("Remove would take out: id 11 (row 10). Not found, so not removed: %s.", typo), fixed = TRUE)
    expect_match(preview, "type it with a leading #", fixed = TRUE)
    session$setInputs(remove_go = 1L)
    expect_identical(vapply(state$removed_cases %||% list(), function(x) x$row, integer(1)), 10L)
    result <- sw_text(output$remove_result)
    expect_match(result, sprintf("Not found among the respondents in the analysis, so not removed: %s.", typo), fixed = TRUE)
    expect_match(result, sprintf("(for example #%s)", typo), fixed = TRUE)
    expect_match(sw_text(output$remove_hint), "type it with a leading # (#17)", fixed = TRUE)
  })
  # Without an identifier column every entry is a row, with or without #.
  shiny::isolate({
    roles <- sieve_roles_table(st$raw, st$items, st$roles)
    roles$role[roles$variable == "id"] <- "auxiliary"
    st$roles <- roles
  })
  no_id <- shiny::isolate(sieve_case_ids(st))
  expect_identical(sieve_case_parse("6 #7", no_id)$rows, c(6L, 7L))
  expect_null(sieve_case_parse("6", no_id)$hint)
})

test_that("a flagged respondent's responses are read beside the flag, and the button opens them in the Observations table", {
  st <- ns_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    r <- careless()
    flagged <- careless_flagged()
    expect_gt(nrow(flagged), 0L)
    session$setInputs(careless_tbl_rows_selected = 1L)
    detail <- sw_text(output$careless_detail)
    expect_match(detail, sprintf("Responses on the %d screened items, in the order screened:", length(r$items)), fixed = TRUE)
    at <- match(flagged$row[1], sieve_case_index(state))
    first <- r$items[[1L]]
    expect_match(detail, sprintf("%s %s", first, format(state$raw[[first]][[at]], trim = TRUE)), fixed = TRUE)
    expect_match(detail, "Show in the Observations table", fixed = TRUE)
    session$setInputs(open_responses = 1L)
    expect_identical(opened(), flagged$row[1])
    expect_match(sw_text(output$case_grid_note),
      sprintf("Opened from the Careless responding panel for original row %d, with the screened items as the response columns", flagged$row[1]),
      fixed = TRUE
    )
  })
})

test_that("the study's sampling design is declared on Ingest step 4, logged, and stated read-only on the SEM page", {
  ingest <- paste(as.character(mod_ingest_ui("ingest")), collapse = "")
  expect_match(ingest, "Sampling design of the study (every page reads it)", fixed = TRUE)
  expect_true(grepl('id="ingest-sampling_unit"', ingest, fixed = TRUE))
  expect_true(grepl('id="ingest-confirm_button"', ingest, fixed = TRUE))
  sem <- paste(as.character(mod_sem_ui("sem")), collapse = "")
  expect_false(grepl('id="sem-sampling_unit"', sem, fixed = TRUE))
  st <- ns_hs9_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    before <- state$sampling_design
    session$setInputs(sampling_unit = "independent", sampling_cluster = "")
    expect_identical(state$sampling_design, before)
    session$setInputs(sampling_unit = "clustered", sampling_cluster = "school")
    expect_identical(state$sampling_design, list(unit = "clustered", cluster = "school"))
    expect_match(sw_text(output$sampling_box), "In force: clustered rows, clusters given by school", fixed = TRUE)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "Set the study's sampling design: clustered rows, clusters given by school", fixed = TRUE)
  })
  shiny::testServer(mod_sem_server, args = list(state = st), {
    expect_match(sw_text(output$sampling_note),
      "Sampling design of the study: clustered rows, clusters given by school (set on the Ingest tab, step 4, for every page).",
      fixed = TRUE
    )
  })
  expect_identical(sieve_sampling_text(list(unit = "repeated", cluster = "")),
    "repeated observations, with no cluster or respondent column chosen yet")
  expect_identical(sieve_sampling_text(list(unit = "independent")), "independent rows")
})

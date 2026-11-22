# State integrity: one transformation service for reverse scoring, undo
# records and optimizer candidates bound to the dataset and revision they
# were taken from.
#
# The expected values in the service tests are written out by hand or
# derived from the algebra of a reflection (x + x' = lo + hi), never from
# the function under test. The module tests drive the real observers with
# shiny::testServer, and the cross-dataset tests load each dataset through
# the real load observer.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

# A loaded, confirmed state on the primary nine Holzinger and Swineford
# tests, assembled the way the other module tests assemble it.
si_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

# A small item pool with a ledger: `a` an integer seven point item with a
# missing response, `b` a non-integer item, `g` a text column.
si_pool <- function() {
  d <- data.frame(
    a = c(1L, 2L, 7L, NA, 4L, 6L),
    b = c(0.25, 1.5, 3.75, 2, NA, 0.5),
    stringsAsFactors = FALSE
  )
  list(data = d, meta = sieve_build_meta(d, c("a", "b")))
}

si_quiet <- function(expr) invisible(utils::capture.output(expr))

# The Ingest and optimizer modules on one shared state, wired as the
# application server wires them. Datasets are loaded by the real load
# observer; the other tabs are left out because each would refit its models
# at every step of a test.
# Bounds are declared in an item's Response bounds cell (cell_edit, field
# "bounds") and in the Response bounds row above the item grid (bounds_lo,
# bounds_hi, bounds_scope, bounds_apply, bounds_clear); items are reversed
# by an item's Reverse? cell and the Reverse? row (keying_scope, keying_to,
# keying_apply), all inputs of the Ingest module itself. Reverse? No undoes
# a reversal, and undo records with origin "prepare" (a saved study may
# hold some) are presented to the service the Reverse? cell and row call,
# directly.
si_server <- function(input, output, session) {
  state <- sieve_new_state()
  mod_ingest_server("ingest", state)
  mod_optimize_server("optimize", state)
}

# Everything in the shared state that a refused or stale action must leave
# exactly as it was.
si_snapshot <- function(state) {
  shiny::isolate(list(
    items = state$items, meta = state$meta, raw = state$raw,
    transforms = state$transforms, codelog = state$codelog,
    dataset_id = state$dataset_id
  ))
}

# The cells of one item's row of the item grid as drawn (output$type_grid),
# named by the grid's columns (sieve_grid_columns()).
si_grid_cells <- function(out, item) {
  x <- jsonlite::fromJSON(as.character(out), simplifyVector = FALSE)$x
  cols <- SIEVER:::sieve_grid_columns()
  text <- function(v) if (is.null(v)) NA_character_ else as.character(v)
  variable <- vapply(x$data[[match("Variable", cols)]], text, character(1))
  row <- which(variable == item)
  stats::setNames(lapply(x$data, function(col) text(col[[row]])), cols[seq_along(x$data)])
}

# The cells of a row pushed to the item grid in place (the message
# sieve_session_message() sends), named by the grid's columns.
si_pushed_cells <- function(message) {
  cols <- SIEVER:::sieve_grid_columns()
  cells <- message$cells
  stats::setNames(lapply(cells, as.character), cols[as.integer(names(cells)) + 1L])
}

# ---- The transformation service: plain functions ---------------------------

test_that("a reversal reflects within the declared bounds and changes the data and the ledger together", {
  p <- si_pool()
  r <- SIEVER:::sieve_reverse_item(p$data, p$meta, "a", lo = 1, hi = 7)

  # Written out by hand: 8 - x on a 1 to 7 scale; the missing response stays missing.
  expect_identical(r$data$a, c(7, 6, 1, NA, 4, 2))
  expect_identical(r$data$b, p$data$b) # no other item is touched
  expect_identical(names(r$data), names(p$data))

  row <- r$meta[r$meta$item == "a", ]
  expect_true(row$reverse)
  expect_identical(row$rev_lo, 1)
  expect_identical(row$rev_hi, 7)
  expect_identical(row$rev_bounds, "declared")
  expect_identical(row$note, "reverse coded")
  # The summaries follow from the algebra of a reflection, x' = (lo + hi) - x:
  # the mean reflects, the standard deviation is unchanged, and the extremes
  # trade places.
  before <- p$meta[p$meta$item == "a", ]
  expect_equal(row$mean, (1 + 7) - before$mean, tolerance = 1e-14)
  expect_equal(row$sd, before$sd, tolerance = 1e-14)
  expect_identical(row$min, (1 + 7) - before$max)
  expect_identical(row$max, (1 + 7) - before$min)
  # The other ledger row is exactly as it was.
  expect_identical(r$meta[r$meta$item == "b", ], sieve_meta_keying(p$meta)[p$meta$item == "b", ])

  rec <- r$record
  expect_s3_class(rec, "sieve_transform")
  expect_identical(
    rec[c("type", "item", "lo", "hi", "bounds", "from", "to")],
    list(
      type = "reverse", item = "a", lo = 1, hi = 7, bounds = "declared",
      from = FALSE, to = TRUE
    )
  )
  expect_identical(rec$n, 6L)
  expect_identical(rec$n_missing, 1L)
  # The inputs are not modified (the function works on copies).
  expect_identical(p$data$a, c(1L, 2L, 7L, NA, 4L, 6L))
  expect_false(isTRUE(p$meta$reverse[1]))
})

test_that("a refreshed ledger row is identical to the row a ledger built from the same values would hold", {
  x <- withr::with_seed(113, stats::runif(301, 0.3, 9.7))
  x[c(5, 40)] <- NA
  d <- data.frame(v = x, k = 4, e = NA_real_, one = c(2.5, rep(NA, 300)))
  built <- sieve_build_meta(d, names(d))
  for (it in names(d)) {
    expect_identical(SIEVER:::sieve_item_summary(d[[it]]),
      as.list(built[built$item == it, c("mean", "sd", "min", "max")]) |> lapply(unname),
      info = it
    )
  }
  # After a reversal the row is the row of a ledger built from the reversed data.
  r <- SIEVER:::sieve_reverse_item(d, built, "v", lo = 0, hi = 10)
  rebuilt <- sieve_build_meta(r$data, names(d))
  expect_identical(
    r$meta[1, c("mean", "sd", "min", "max", "n_missing", "n_levels")],
    rebuilt[1, c("mean", "sd", "min", "max", "n_missing", "n_levels")]
  )
})

test_that("reversing twice restores integer, non-integer and missing values exactly", {
  p <- si_pool()
  # Integer responses: the second subtraction is itself exact.
  once <- SIEVER:::sieve_reverse_item(p$data, p$meta, "a", lo = 1, hi = 7)
  twice <- SIEVER:::sieve_reverse_item(once$data, once$meta, "a")
  expect_identical(twice$data$a, as.numeric(p$data$a))
  expect_identical(is.na(twice$data$a), is.na(p$data$a))
  expect_false(twice$meta$reverse[twice$meta$item == "a"])
  expect_true(is.na(twice$meta$note[twice$meta$item == "a"]))
  # With the values as loaded at hand the original column returns, storage
  # type included.
  once <- SIEVER:::sieve_reverse_item(p$data, p$meta, "a", lo = 1, hi = 7, loaded = p$data$a)
  twice <- SIEVER:::sieve_reverse_item(once$data, once$meta, "a", loaded = p$data$a)
  expect_identical(twice$data, p$data)
  expect_identical(once$record$applied, TRUE)
  expect_identical(twice$record$applied, FALSE)
  expect_identical(twice$record$after_key, once$record$before_key)

  # Non-integer values. Subtraction alone does not give the values back:
  # this is the case the service has to handle, shown here on the test
  # vector itself so the test cannot pass by accident.
  x <- withr::with_seed(113, stats::runif(400, 0.3, 9.7))
  x[c(7, 120)] <- NA
  s <- min(x, na.rm = TRUE) + max(x, na.rm = TRUE)
  plain <- s - (s - x)
  expect_false(identical(plain, x))
  expect_gt(sum(plain != x, na.rm = TRUE), 20)
  expect_lt(max(abs(plain - x), na.rm = TRUE), 4 * .Machine$double.eps * s)

  d <- data.frame(v = x, w = rev(x))
  m <- sieve_build_meta(d, c("v", "w"))
  once <- SIEVER:::sieve_reverse_item(d, m, "v", loaded = x)
  expect_identical(once$data$v, s - x) # one reflection of the values as loaded
  expect_identical(once$record$bounds, "observed")
  twice <- SIEVER:::sieve_reverse_item(once$data, once$meta, "v", loaded = x)
  expect_identical(twice$data$v, x) # exact, where subtraction is not
  expect_identical(twice$data, d)
  thrice <- SIEVER:::sieve_reverse_item(twice$data, twice$meta, "v", loaded = x)
  expect_identical(thrice$data$v, once$data$v) # and the same reflection again
  # Any number of applications leaves the item at the values as loaded or at
  # their single reflection: nothing accumulates.
  cur <- list(data = d, meta = m)
  for (i in 1:9) cur <- SIEVER:::sieve_reverse_item(cur$data, cur$meta, "v", loaded = x)
  expect_identical(cur$data$v, s - x)

  # Without the values as loaded the second application is a subtraction:
  # equal to rounding, and the record says the exact state is not known.
  blind <- SIEVER:::sieve_reverse_item(once$data, once$meta, "v")
  expect_equal(blind$data$v, x, tolerance = 1e-15)
  expect_true(is.na(blind$record$applied))
  expect_identical(blind$record$basis, "current")
})

test_that("bounds are honored: declared over observed, stored and reused, never inferred silently", {
  d <- data.frame(q = c(2, 3, 6, NA, 4)) # observed range 2 to 6 on a 1 to 7 scale
  m <- sieve_build_meta(d, "q")

  # No bounds anywhere: the observed range is used, and said to be.
  obs <- SIEVER:::sieve_reverse_item(d, m, "q")
  expect_identical(obs$data$q, c(6, 5, 2, NA, 4)) # (2 + 6) - x
  expect_identical(obs$record$bounds, "observed")
  expect_false(obs$record$from_ledger)
  expect_identical(obs$meta$rev_bounds, "observed")
  expect_identical(c(obs$meta$rev_lo, obs$meta$rev_hi), c(2, 6))
  expect_match(obs$note, "the observed range, because no response bounds were declared", fixed = TRUE)
  expect_match(SIEVER:::sieve_reverse_message(obs$record), "No response bounds were declared", fixed = TRUE)

  # Declared bounds are used, not the observed range.
  dec <- SIEVER:::sieve_reverse_item(d, m, "q", lo = 1, hi = 7)
  expect_identical(dec$data$q, c(6, 5, 2, NA, 4)) # (1 + 7) - x happens to coincide here ...
  d2 <- data.frame(q = c(2, 3, 5, NA, 4))
  m2 <- sieve_build_meta(d2, "q")
  dec <- SIEVER:::sieve_reverse_item(d2, m2, "q", lo = 1, hi = 7)
  expect_identical(dec$data$q, c(6, 5, 3, NA, 4)) # ... and does not here: 8 - x, not 7 - x
  expect_identical(dec$record$bounds, "declared")

  # Bounds declared in the ledger are used when none are passed: the observed
  # range (2 to 5) is not consulted.
  m3 <- sieve_meta_keying(m2)
  m3$rev_lo <- 1
  m3$rev_hi <- 7
  m3$rev_bounds <- "declared"
  led <- SIEVER:::sieve_reverse_item(d2, m3, "q")
  expect_identical(led$data$q, c(6, 5, 3, NA, 4))
  expect_true(led$record$from_ledger)
  expect_identical(led$record$bounds, "declared")

  # The second application uses the stored bounds even when the observed
  # range has changed in between (the response at the low end is now
  # missing) and even when other bounds are passed; those are reported.
  moved <- obs$data
  moved$q[3] <- NA # observed range is now 4 to 6
  back <- SIEVER:::sieve_reverse_item(moved, obs$meta, "q", lo = 0, hi = 10)
  expect_identical(back$data$q, c(2, 3, NA, NA, 4)) # (2 + 6) - x again, not (4 + 6) - x or 10 - x
  expect_identical(c(back$record$lo, back$record$hi), c(2, 6))
  expect_identical(back$record$unused, c(lo = 0, hi = 10))
  expect_match(SIEVER:::sieve_reverse_message(back$record), "The bounds entered (0 to 10) were not used",
    fixed = TRUE
  )
  # The stored bounds stay with the item, so a third application repeats the first.
  expect_identical(c(back$meta$rev_lo, back$meta$rev_hi, back$meta$rev_bounds), c("2", "6", "observed"))
  again <- SIEVER:::sieve_reverse_item(back$data, back$meta, "q")
  expect_identical(again$data$q, c(6, 5, NA, NA, 4))
  # New bounds may be declared for an item that is not reverse scored.
  redeclared <- SIEVER:::sieve_reverse_item(back$data, back$meta, "q", lo = 1, hi = 7)
  expect_identical(redeclared$data$q, c(6, 5, NA, NA, 4))
  expect_identical(redeclared$meta$rev_bounds, "declared")
  expect_identical(c(redeclared$meta$rev_lo, redeclared$meta$rev_hi), c(1, 7))
})

test_that("values outside the bounds are reported and nothing is changed", {
  d <- data.frame(q = c(1, 5, 9, 3, 99, 9)) # 9 and 99 are not on a 1 to 5 scale
  m <- sieve_build_meta(d, "q")
  err <- tryCatch(SIEVER:::sieve_reverse_item(d, m, "q", lo = 1, hi = 5), error = function(e) e)
  expect_s3_class(err, "sieve_transform_error")
  expect_match(conditionMessage(err), "3 values of item \"q\" lie outside the response bounds 1 to 5 (9, 99)",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "nothing was changed", fixed = TRUE)
  expect_identical(SIEVER:::sieve_outside_bounds(d$q, 1, 5), c(3L, 5L, 6L))
  # Missing values are not outside, and the reflection of an in-range value
  # is never reported, whatever the rounding of the subtraction.
  expect_identical(SIEVER:::sieve_outside_bounds(c(NA, 1, 5), 1, 5), integer(0))
  x <- withr::with_seed(113, stats::runif(2000, 0.1, 0.7))
  lo <- min(x)
  hi <- max(x)
  expect_identical(SIEVER:::sieve_outside_bounds((lo + hi) - x, lo, hi), integer(0))
  expect_identical(SIEVER:::sieve_outside_bounds(c(lo - 1e-9, hi + 1e-9), lo, hi), 1:2)
})

test_that("an unknown item, a non-numeric item and unusable bounds are refused with a clear message", {
  p <- si_pool()
  refuse <- function(expr, pattern) {
    err <- tryCatch(expr, error = function(e) e)
    expect_s3_class(err, "sieve_transform_error")
    expect_match(conditionMessage(err), pattern, fixed = TRUE)
  }
  refuse(SIEVER:::sieve_reverse_item(p$data, p$meta, "zz"), "Item \"zz\" is not in the item data")
  refuse(
    SIEVER:::sieve_reverse_item(p$data, p$meta[p$meta$item != "a", ], "a"),
    "Item \"a\" is not in the item ledger"
  )
  refuse(
    SIEVER:::sieve_reverse_item(p$data, rbind(p$meta, p$meta[1, ]), "a"),
    "listed 2 times in the item ledger"
  )
  refuse(SIEVER:::sieve_reverse_item(p$data, p$meta, c("a", "b")), "One item name is needed")

  mixed <- data.frame(
    txt = c("1", "2", "3"), fac = factor(c("lo", "mid", "hi")),
    lgl = c(TRUE, FALSE, TRUE), stringsAsFactors = FALSE
  )
  mm <- sieve_build_meta(mixed, names(mixed))
  refuse(SIEVER:::sieve_reverse_item(mixed, mm, "txt"), "Item \"txt\" is not numeric (it is stored as character)")
  refuse(SIEVER:::sieve_reverse_item(mixed, mm, "fac"), "Item \"fac\" is not numeric (it is stored as factor)")
  refuse(SIEVER:::sieve_reverse_item(mixed, mm, "lgl"), "Item \"lgl\" is not numeric (it is stored as logical)")

  refuse(SIEVER:::sieve_reverse_item(p$data, p$meta, "a", lo = 1), "Give both response bounds")
  refuse(SIEVER:::sieve_reverse_item(p$data, p$meta, "a", lo = 1, hi = NA_real_), "Give both response bounds")
  refuse(SIEVER:::sieve_reverse_item(p$data, p$meta, "a", lo = 7, hi = 1), "need a minimum below the maximum")
  refuse(SIEVER:::sieve_reverse_item(p$data, p$meta, "a", lo = 3, hi = 3), "need a minimum below the maximum")
  refuse(SIEVER:::sieve_reverse_item(p$data, p$meta, "a", loaded = 1:3), "values as loaded")

  flat <- data.frame(k = c(4, 4, 4, NA), e = NA_real_)
  fm <- sieve_build_meta(flat, c("k", "e"))
  refuse(SIEVER:::sieve_reverse_item(flat, fm, "k"), "takes a single value (4)")
  refuse(SIEVER:::sieve_reverse_item(flat, fm, "e"), "has no observed values")
  # With declared bounds both are well defined.
  expect_identical(SIEVER:::sieve_reverse_item(flat, fm, "k", lo = 1, hi = 5)$data$k, c(2, 2, 2, NA))
  expect_identical(SIEVER:::sieve_reverse_item(flat, fm, "e", lo = 1, hi = 5)$data$e, rep(NA_real_, 4))
})

test_that("recorded bounds are read back exactly by the R parser", {
  awkward <- c(
    7, -3, 0, 0.1, 2.25, 1 / 3, 2 / 3, 0.66666667, 1e-20, 123456789.123456789,
    2^53 - 1, 1e15, 1e22, .Machine$double.xmin, 100000, 0.30000000000000004,
    withr::with_seed(113, stats::runif(500, -50, 50))
  )
  lit <- SIEVER:::sieve_num_lit(awkward)
  back <- vapply(lit, function(s) eval(parse(text = s)), numeric(1), USE.NAMES = FALSE)
  expect_identical(back, awkward)
  # Ordinary bounds stay ordinary decimal constants; a value that 15 digits
  # do not identify is written as a hexadecimal constant (1/3 is
  # 0x1.5555555555555 times 2^-2).
  expect_identical(
    lit[c(1, 2, 3, 4, 5, 8, 11, 15)],
    c("7", "-3", "0", "0.1", "2.25", "0.66666667", "9007199254740991", "100000")
  )
  expect_identical(lit[6], "0x1.5555555555555p-2")
  # Why not seventeen decimal digits: they need not come back as the same
  # double. The check is on what the parser returns, so it holds wherever
  # the test runs; where the parser is exact the set below is simply empty.
  seventeen <- sprintf("%.17g", awkward)
  misread <- vapply(seventeen, function(s) eval(parse(text = s)), numeric(1), USE.NAMES = FALSE) != awkward
  expect_identical(back[misread], awkward[misread])
  # Six significant digits in the recorded step would not be the bound.
  expect_false(identical(as.numeric(sprintf("%g", 1 / 3)), 1 / 3))
  # Text for a note is the decimal digits of the number.
  expect_identical(SIEVER:::sieve_num_text(c(7, 0.1, 1 / 3, 100000)), c("7", "0.1", sprintf("%.16g", 1 / 3), "100000"))

  # A reversal whose observed bounds need every digit is replayed exactly.
  x <- c(1 / 3, 2 / 3, 5 / 7, 9.7 / 3, NA)
  d <- data.frame(v = x)
  r <- SIEVER:::sieve_reverse_item(d, sieve_build_meta(d, "v"), "v", loaded = x)
  env <- new.env(parent = globalenv())
  env$items <- d
  eval(parse(text = paste(r$code, collapse = "\n")), envir = env)
  expect_identical(env$items$v, r$data$v)
  expect_identical(r$data$v, (1 / 3 + 9.7 / 3) - x)
})

test_that("the recorded R reproduces the operation on every view of the item data a later step reads", {
  x <- withr::with_seed(113, stats::runif(60, 0.3, 9.7))
  d <- data.frame(id = seq_along(x), v = x, w = rev(x), grp = rep(c("a", "b"), 30))
  m <- sieve_build_meta(d, c("v", "w"))
  r <- SIEVER:::sieve_reverse_item(d[, c("v", "w")], m, "v", loaded = d$v)

  env <- new.env(parent = globalenv())
  env$d <- d
  env$items <- d[, c("v", "w")]
  env$items_data <- env$items
  env$active_items <- env$items
  env$X <- env$active_items
  env$items_df <- env$items[, "v", drop = FALSE]
  env$inv_data <- d[seq(1, 60, by = 2), c("v", "w", "grp")] # a view with fewer rows
  env$other <- data.frame(w = 1:3) # holds no column v
  eval(parse(text = paste(r$code, collapse = "\n")), envir = env)

  expect_identical(env$items, r$data) # bit for bit what the app holds
  expect_identical(env$items_data$v, r$data$v)
  expect_identical(env$active_items$v, r$data$v)
  expect_identical(env$X$v, r$data$v)
  expect_identical(env$items_df$v, r$data$v)
  expect_identical(env$inv_data$v, r$data$v[seq(1, 60, by = 2)])
  expect_identical(env$inv_data$w, d$w[seq(1, 60, by = 2)])
  expect_identical(env$d, d) # the data as read are not changed
  expect_identical(env$other, data.frame(w = 1:3))
  # A script that has not built the views yet still runs.
  bare <- new.env(parent = globalenv())
  bare$items <- d[, c("v", "w")]
  eval(parse(text = paste(r$code, collapse = "\n")), envir = bare)
  expect_identical(bare$items, r$data)
  # Every view the code names is a view of the item data.
  for (view in SIEVER:::sieve_item_views()) {
    expect_true(any(grepl(sprintf("\"%s\"", view), r$code, fixed = TRUE)), info = view)
  }
})

# ---- The service applied to the shared state -------------------------------

test_that("the state changes together or not at all, and the data as loaded are never written", {
  st <- si_state()
  raw_before <- shiny::isolate(st$raw)
  shiny::isolate({
    # One item cannot be reversed (a value is outside the bounds given), the
    # other can: the batch applies the second and reports the first.
    out <- SIEVER:::sieve_apply_reverse(st, c("t2_cubes", "t7_sentence"), lo = 4, hi = 30, origin = "ai")
  })
  expect_named(out$applied, "t7_sentence") # observed 4 to 28, inside 4 to 30
  expect_named(out$refused, "t2_cubes") # observed 9 to 37
  expect_match(out$refused[["t2_cubes"]], "outside the response bounds 4 to 30", fixed = TRUE)
  now <- si_snapshot(st)
  expect_identical(now$raw, raw_before)
  expect_identical(now$items$t2_cubes, raw_before$t2_cubes)
  expect_identical(now$items$t7_sentence, (4 + 30) - as.numeric(raw_before$t7_sentence))
  expect_false(now$meta$reverse[now$meta$item == "t2_cubes"])
  expect_true(now$meta$reverse[now$meta$item == "t7_sentence"])
  # One record, bound to the dataset and to the revision it produced.
  expect_named(now$transforms, "T1")
  rec <- now$transforms$T1
  expect_identical(
    rec[c("id", "item", "origin", "dataset_id")],
    list(id = "T1", item = "t7_sentence", origin = "ai", dataset_id = now$dataset_id)
  )
  expect_true(SIEVER:::sieve_same_revision(rec$revision, shiny::isolate(SIEVER:::sieve_revision(st))))
  expect_false(SIEVER:::sieve_same_revision(rec$revision_before, rec$revision, what = "data"))
  expect_false(SIEVER:::sieve_same_revision(rec$revision_before, rec$revision, what = "meta"))
  expect_true(any(grepl("items[[\"t7_sentence\"]] <- (4 + 30) - items[[\"t7_sentence\"]]",
    now$codelog,
    fixed = TRUE
  )))

  # A request in which nothing can be done changes nothing at all.
  before <- si_snapshot(st)
  shiny::isolate(out <- SIEVER:::sieve_apply_reverse(st, c("no_such_item", "t2_cubes"), lo = 10, hi = 20))
  expect_length(out$applied, 0L)
  expect_named(out$refused, c("no_such_item", "t2_cubes"))
  expect_identical(si_snapshot(st), before)
})

test_that("a hand-built state is given a dataset identity when a record first needs one", {
  st <- si_state()
  st$dataset_id <- NULL
  st$dataset_seq <- 0L
  st$scientific_dataset_id <- NULL
  expect_null(shiny::isolate(st$dataset_id))
  shiny::isolate(SIEVER:::sieve_apply_reverse(st, "t2_cubes"))
  expect_identical(shiny::isolate(st$dataset_id), "D1")
  expect_identical(shiny::isolate(SIEVER:::sieve_ensure_dataset(st)), "D1") # and keeps it
  expect_identical(shiny::isolate(st$dataset_seq), 1L)
})

test_that("a stored result applies only to the inputs it was computed from", {
  st <- si_state()
  shiny::isolate({
    SIEVER:::sieve_new_dataset(st)
    made <- SIEVER:::sieve_revision(st)
    expect_null(SIEVER:::sieve_stale_reason(made, SIEVER:::sieve_revision(st)))
    expect_match(SIEVER:::sieve_stale_reason(NULL, made), "not recorded")

    st$meta$status[1] <- "parked"
    ledger <- SIEVER:::sieve_revision(st)
    expect_match(SIEVER:::sieve_stale_reason(made, ledger), "item ledger has changed")
    expect_null(SIEVER:::sieve_stale_reason(made, ledger, what = c("dataset", "data")))
    # A result applies again once the inputs are what they were.
    st$meta$status[1] <- "active"
    expect_null(SIEVER:::sieve_stale_reason(made, SIEVER:::sieve_revision(st)))

    st$items[[1]][1] <- st$items[[1]][1] + 1
    expect_match(SIEVER:::sieve_stale_reason(made, SIEVER:::sieve_revision(st)), "item data have changed")
    st$items[[1]][1] <- st$items[[1]][1] - 1

    # The same names and the same values in a newly loaded dataset are still
    # a different dataset.
    SIEVER:::sieve_new_dataset(st)
    reloaded <- SIEVER:::sieve_revision(st)
    expect_identical(reloaded[c("data_key", "meta_key")], made[c("data_key", "meta_key")])
    expect_match(SIEVER:::sieve_stale_reason(made, reloaded), "different dataset")

    # A result that changes the ledger also applies to the ledger its own
    # last application produced, and to nothing else.
    result <- list(revision = made, applied_revision = NULL)
    expect_match(SIEVER:::sieve_result_stale(result, reloaded), "different dataset")
    result <- list(revision = reloaded, applied_revision = NULL)
    expect_null(SIEVER:::sieve_result_stale(result, reloaded))
    st$meta$status[2] <- "parked"
    after_apply <- SIEVER:::sieve_revision(st)
    expect_match(SIEVER:::sieve_result_stale(result, after_apply), "item ledger has changed")
    result$applied_revision <- after_apply
    expect_null(SIEVER:::sieve_result_stale(result, after_apply))
    expect_null(SIEVER:::sieve_result_stale(result, reloaded))
    st$meta$status[3] <- "parked"
    expect_match(SIEVER:::sieve_result_stale(result, SIEVER:::sieve_revision(st)), "item ledger has changed")
  })
})

# ---- One reversal, whichever control asks for it ---------------------------

test_that("the Ingest cell and the grid's Reverse? row reverse items identically", {
  # The second route compared with the cell is the Reverse? row above the
  # item grid, which reverses the included items of a scope at once; here
  # the scope is the construct Speed, so the cell reverses the same three
  # items one at a time.
  speed <- c("t10_addition", "t12_counting_groups_of_dots", "t13_straight_and_curved_capitals")
  it <- speed[1]
  fx <- sieve_hs9_fixture()
  loaded <- fx$data[[it]]
  expect_identical(range(loaded), c(30L, 171L))
  # The observed range of each, written out: 30 to 171, 61 to 200, 100 to 333.
  sums <- c(201, 261, 433)

  # The rows the grid is sent in place (sieve_session_message()), captured.
  pushed <- list()
  local_mocked_bindings(sieve_session_message = function(session, type, message) {
    # One message carries every row an edit or an action changed.
    if (identical(type, "sieve_ingest_row")) for (row in message$rows) pushed[[length(pushed) + 1L]] <<- row
    invisible(NULL)
  }, .package = "SIEVER")

  via_ingest <- si_state()
  shiny::testServer(mod_ingest_server, args = list(state = via_ingest), {
    for (k in seq_along(speed)) {
      session$setInputs(cell_edit = list(item = speed[k], field = "reverse", value = "Yes"))
    }
  })
  via_row <- si_state()
  shiny::testServer(mod_ingest_server, args = list(state = via_row), {
    # The grid as drawn before: Reverse? No, and an empty Response bounds
    # cell whose hint is the observed range (no bounds are declared and the
    # fixture carries no value labels).
    cells <- si_grid_cells(output$type_grid, it)
    expect_match(cells[["Reverse"]], 'value="No" selected>', fixed = TRUE)
    expect_match(cells[["Bounds"]], 'data-field="bounds" value="" placeholder="observed 30 to 171"', fixed = TRUE)
    pushed <<- list()
    session$setInputs(types_scope = "construct:Speed", keying_to = "Yes")
    session$setInputs(keying_apply = 1L)
    # Each reversed row is pushed to the grid: Reverse? Yes, and the bounds
    # of the reversal, said to be the observed range. Its observed range
    # differs from the fixture's common one, so the cell keeps the range
    # warning, with what to do.
    expect_identical(vapply(pushed, function(m) m$key, character(1)), speed)
    row <- si_pushed_cells(pushed[[1]])
    expect_match(row[["Reverse"]], 'value="Yes" selected>', fixed = TRUE)
    expect_identical(
      row[["Bounds"]],
      paste0(
        '<div class="small">30 to 171</div>',
        '<div class="small text-muted">the bounds of its reversal, the observed range (none were declared); ',
        "set Reverse? to No to change them</div>",
        '<div class="small text-warning-emphasis">observed 30 to 171; the study\'s common range is 0 to 19 ',
        "and no bounds are declared: set Reverse? to No, declare its bounds, and reverse it again</div>"
      )
    )
    # The note above the grid says what was done and which bounds were used.
    expect_identical(grid_note()$kind, "warning")
    expect_match(grid_note()$text, "t10_addition is now reverse scored within its observed range, 30 to 171.", fixed = TRUE)
  })

  a <- si_snapshot(via_ingest)
  b <- si_snapshot(via_row)
  # The same values: one reflection within the observed range, written out.
  expect_identical(a$items[[it]], 201 - as.numeric(loaded))
  for (k in seq_along(speed)) {
    expect_identical(a$items[[speed[k]]], sums[k] - as.numeric(fx$data[[speed[k]]]), info = speed[k])
  }
  expect_identical(b$items, a$items)
  # The same ledger, keying and bounds included (the reverse flag set and
  # the summaries current).
  expect_identical(b$meta, a$meta)
  row <- a$meta[a$meta$item == it, ]
  expect_true(row$reverse)
  expect_identical(c(row$rev_lo, row$rev_hi), c(30, 171))
  expect_identical(row$rev_bounds, "observed")
  expect_equal(row$mean, 201 - mean(loaded), tolerance = 1e-13)
  expect_equal(row$sd, stats::sd(loaded), tolerance = 1e-13)
  expect_identical(c(row$min, row$max), c(30, 171))
  # Both routes withdraw the confirmation of the item types (a reversal
  # changes values the confirmation rested on).
  expect_false(isTRUE(shiny::isolate(via_ingest$confirmed)))
  expect_false(isTRUE(shiny::isolate(via_row$confirmed)))
  # The same recorded steps, one per item (the step is picked out by its
  # stamp and compared without its number).
  step <- function(log, item) {
    at <- grep(sprintf("Reverse score %s within", item), log, fixed = TRUE)
    end <- at + which(log[at:length(log)] == "")[1] - 2L
    sub("^# \\[[0-9]+\\] ", "# ", log[at:end])
  }
  # The logged step is the stamp and exactly the R of the record (the step
  # keeps the values as read aside before it reflects, so a later undo can
  # restore them exactly; see sieve_reverse_code()).
  expect_identical(step(a$codelog, it)[-1], SIEVER:::sieve_reverse_code(a$transforms$T1))
  expect_true(any(grepl("values_as_read[[\"t10_addition\"]] <- items[[\"t10_addition\"]]",
    step(a$codelog, it),
    fixed = TRUE
  )))
  expect_identical(names(a$transforms), c("T1", "T2", "T3"))
  expect_identical(names(b$transforms), names(a$transforms))
  for (k in seq_along(speed)) {
    id <- sprintf("T%d", k)
    expect_identical(step(b$codelog, speed[k]), step(a$codelog, speed[k]), info = speed[k])
    expect_identical(step(b$codelog, speed[k])[-1], SIEVER:::sieve_reverse_code(b$transforms[[id]]), info = speed[k])
    expect_identical(
      a$transforms[[id]][c("item", "lo", "hi", "bounds", "applied", "after_key")],
      b$transforms[[id]][c("item", "lo", "hi", "bounds", "applied", "after_key")],
      info = speed[k]
    )
  }
  expect_identical(a$transforms$T1$item, it)
  # The row is a control of the grid, and its records say so: origin
  # "ingest", as the cell's (SIEVER makes no record with origin "prepare"
  # or "ai"; such records are read from a saved study).
  expect_identical(vapply(a$transforms, function(r) r$origin, character(1), USE.NAMES = FALSE), rep("ingest", 3))
  expect_identical(vapply(b$transforms, function(r) r$origin, character(1), USE.NAMES = FALSE), rep("ingest", 3))
  # Neither route writes the data as loaded.
  expect_identical(a$raw, fx$data)
  expect_identical(b$raw, fx$data)

  # Setting the cells, or the row, back to No restores the loaded values and
  # summaries exactly; the bounds stay with the items.
  shiny::testServer(mod_ingest_server, args = list(state = via_ingest), {
    for (k in seq_along(speed)) {
      session$setInputs(cell_edit = list(item = speed[k], field = "reverse", value = "No"))
    }
  })
  shiny::testServer(mod_ingest_server, args = list(state = via_row), {
    session$setInputs(types_scope = "construct:Speed", keying_to = "No")
    session$setInputs(keying_apply = 1L)
    expect_match(grid_note()$text,
      "t10_addition is no longer reverse scored: its values are again the values as loaded.",
      fixed = TRUE
    )
  })
  back <- si_snapshot(via_ingest)
  expect_identical(back$items, fx$data[, fx$item_cols, drop = FALSE])
  fresh <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  same <- setdiff(names(fresh), c("rev_lo", "rev_hi", "rev_bounds"))
  expect_identical(back$meta[, same], fresh[, same])
  expect_identical(back$meta$rev_bounds[match(speed, back$meta$item)], rep("observed", 3))
  expect_identical(
    back$transforms$T4[c("item", "from", "to", "applied")],
    list(item = it, from = TRUE, to = FALSE, applied = FALSE)
  )
  back_row <- si_snapshot(via_row)
  expect_identical(back_row$items, back$items)
  expect_identical(back_row$meta, back$meta)
  for (id in c("T4", "T5", "T6")) {
    expect_identical(
      back_row$transforms[[id]][c("item", "from", "to", "applied", "after_key")],
      back$transforms[[id]][c("item", "from", "to", "applied", "after_key")],
      info = id
    )
  }
})

test_that("a reversal the service refuses leaves the Ingest grid's item as it was", {
  st <- si_state()
  shiny::isolate({
    st$items$t2_cubes <- rep(12L, 301L)
    st$raw$t2_cubes <- rep(12L, 301L)
    st$meta <- sieve_build_meta(st$raw, names(st$items), sieve_hs9_fixture()$theory_map)
  })
  before <- si_snapshot(st)
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
  })
  expect_identical(si_snapshot(st), before)
})

test_that("the item specification import reverses through the same service", {
  st <- si_state()
  fx <- sieve_hs9_fixture()
  spec <- data.frame(item = fx$item_cols, reverse = fx$item_cols %in% c("t2_cubes", "t9_word_meaning"))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(spec, path, row.names = FALSE)
  # A specification that marks items as reverse keyed and does not say
  # whether the data already hold the reversal is not acted on (assuming
  # they do not would reverse exported data twice). It is refused and
  # nothing changes ...
  before <- si_snapshot(st)
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(up_spec = list(name = "spec.csv", datapath = path))
  })
  expect_identical(si_snapshot(st), before)
  # ... until the researcher says the loaded data hold the items as
  # administered. Then the reversal goes through the service.
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(spec_keying = "administered")
    session$setInputs(up_spec = list(name = "spec.csv", datapath = path))
  })
  now <- si_snapshot(st)
  expect_identical(now$meta$reverse, spec$reverse)
  expect_identical(now$items$t2_cubes, (9 + 37) - as.numeric(fx$data$t2_cubes))
  expect_identical(now$items$t9_word_meaning, (1 + 43) - as.numeric(fx$data$t9_word_meaning))
  expect_identical(
    vapply(now$transforms, function(r) r$origin, character(1), USE.NAMES = FALSE),
    c("specification", "specification")
  )
  expect_identical(now$meta$rev_bounds[spec$reverse], c("observed", "observed"))
  expect_identical(now$raw, fx$data)
})

test_that("a reversal recorded after an analysis reaches the data that later steps replay", {
  # An analysis step reads X, an item is reversed, a later step reads X
  # again. Replayed, the two steps must see different values, as in the
  # app, so the script agrees with the app. Here the reversals are applied
  # by the real observers, one through an item's Reverse? cell and the
  # others through the Reverse? row above the grid, within the bounds the
  # Response bounds row declared, and one of them is taken back.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9")
    session$setInputs(load = 1)
  })
  shiny::isolate(sieve_log(
    st, "Read two items before any reversal",
    c('before_cubes <- X[["t2_cubes"]]', 'before_addition <- active_items[["t10_addition"]]')
  ))
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
  })
  speed <- c("t10_addition", "t12_counting_groups_of_dots", "t13_straight_and_curved_capitals")
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    # 0 to 200 holds every value of t10_addition and t12_counting_groups_of_dots,
    # and not those of t13_straight_and_curved_capitals (100 to 333), which
    # is left undeclared and so is reversed within its observed range.
    session$setInputs(bounds_lo = 0, bounds_hi = 200, types_scope = "construct:Speed")
    session$setInputs(bounds_apply = 1)
    expect_identical(state$meta$rev_bounds[match(speed, state$meta$item)], c("declared", "declared", NA))
    expect_match(grid_note()$text, "a value outside 0 to 200", fixed = TRUE)
    session$setInputs(types_scope = "construct:Speed", keying_to = "Yes")
    session$setInputs(keying_apply = 1)
    expect_identical(state$meta$reverse[match(speed, state$meta$item)], c(TRUE, TRUE, TRUE))
    expect_identical(state$meta$rev_bounds[match(speed, state$meta$item)], c("declared", "declared", "observed"))
  })
  shiny::isolate(sieve_log(
    st, "Read them again",
    c(
      'after_cubes <- X[["t2_cubes"]]', 'after_addition <- active_items[["t10_addition"]]',
      'after_dots <- X[["t12_counting_groups_of_dots"]]',
      'after_capitals <- X[["t13_straight_and_curved_capitals"]]',
      'cubes_for_omega <- items_df[["t2_cubes"]]',
      'cubes_for_invariance <- inv_data[["t2_cubes"]]'
    )
  ))
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "No"))
  })
  shiny::isolate(sieve_log(st, "And once more", 'last_cubes <- X[["t2_cubes"]]'))

  txt <- NULL
  shiny::testServer(mod_repro_server, args = list(state = st), {
    txt <<- script()
  })
  expect_false(grepl("items_original", txt, fixed = TRUE))
  expect_false(grepl("d$t2_cubes <-", txt, fixed = TRUE)) # no step writes the data as read
  env <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = txt), envir = env))

  fx <- sieve_hs9_fixture()
  cubes <- as.numeric(fx$data$t2_cubes)
  addition <- as.numeric(fx$data$t10_addition)
  expect_identical(as.numeric(env$before_cubes), cubes)
  expect_identical(as.numeric(env$before_addition), addition)
  expect_identical(env$after_cubes, 46 - cubes) # (9 + 37) - x, the observed range
  expect_identical(env$after_addition, 200 - addition) # the declared bounds, 0 to 200
  expect_identical(env$after_dots, 200 - as.numeric(fx$data$t12_counting_groups_of_dots)) # declared too
  expect_identical(env$after_capitals, 433 - as.numeric(fx$data$t13_straight_and_curved_capitals)) # observed, 100 to 333
  expect_identical(env$cubes_for_omega, 46 - cubes)
  expect_identical(env$cubes_for_invariance, 46 - cubes) # no missing values, so every row
  # The undo restores the values as read themselves, storage type included:
  # the integer column of the data, as the app holds it after the undo.
  expect_identical(env$last_cubes, fx$data$t2_cubes)
  expect_identical(as.numeric(env$last_cubes), cubes)
  expect_identical(shiny::isolate(st$items$t2_cubes), fx$data$t2_cubes)
  # The replayed item data are the app's, and the data as read are untouched.
  app <- shiny::isolate(st$items)
  expect_identical(lapply(env$items, as.numeric), lapply(app, as.numeric))
  expect_identical(SIEVER:::sieve_data_key(env$items), SIEVER:::sieve_data_key(app))
  expect_identical(as.data.frame(env$d)[, fx$item_cols], fx$data[, fx$item_cols])
  expect_identical(shiny::isolate(st$raw), fx$data)
})

# ---- An undo record belongs to the dataset it was taken from ---------------

# The grid returns an item with Reverse? No. Undo records with origin
# "prepare" (a saved study may hold some) are undone by the same service,
# so the tests below make them with sieve_apply_reverse() and present them
# to the Reverse? cell and row directly, with that origin.

# ---- An optimizer candidate belongs to the inputs it was computed from ----

# A two construct pool keeps the search to sixteen fits.
si_small_pool <- function() {
  st <- si_state()
  shiny::isolate(st$meta$status[st$meta$construct == "Speed"] <- "parked")
  st
}

test_that("a stale optimizer candidate never changes the ledger", {
  s <- si_state()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(s$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = s), {
    session$setInputs(max_drop = 0L, min_keep = 3L, cap = 1L, ss = FALSE, run = 1L)
    expect_identical(res()$evaluated, 1L)
    expect_identical(res()$revision, SIEVER:::sieve_revision(state))
    # The replacement: nine new item names in the same state fields.
    newnames <- paste0("new_", names(state$items))
    items2 <- state$items
    names(items2) <- newnames
    meta2 <- state$meta
    meta2$item <- newnames
    state$raw <- items2
    state$items <- items2
    state$meta <- meta2
    state$dataset_name <- "different dataset"
    session$flushReact()
    expect_match(stale(), "item data have changed")
    before <- si_snapshot(state)
    session$setInputs(results_rows_selected = 1L, apply = 1L)
    expect_identical(sum(state$meta$status == "active"), 9L) # not 0 of 9
    expect_identical(si_snapshot(state), before)
    expect_match(paste(unlist(output$run_msg), collapse = " "), "cannot be applied")
  })
})

test_that("Apply works on the inputs the search saw and is refused after the ledger or the data change", {
  st <- si_small_pool()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(max_drop = 1, min_keep = 2, ss = FALSE, cap = 200)
    session$setInputs(run = 1)
    r <- res()
    expect_identical(r$evaluated, 16L)
    pool <- state$meta$item[state$meta$construct != "Speed"]
    expect_identical(r$pool, pool)
    expect_null(stale())
    drop_of <- function(row) setdiff(pool, r$kept[[row]])
    expect_length(drop_of(2), 1L)
    # A candidate that drops two items, found by what it drops and not by
    # where the search happens to list it.
    two <- which(vapply(seq_along(r$kept), function(row) length(drop_of(row)), integer(1)) == 2L)[1]
    expect_false(is.na(two))
    expect_length(drop_of(two), 2L)

    # The ledger changed after the search (an item excluded elsewhere): refused.
    start <- state$meta
    state$meta$status[state$meta$item == pool[6]] <- "parked"
    session$flushReact()
    expect_match(stale(), "item ledger has changed")
    before <- si_snapshot(state)
    session$setInputs(results_rows_selected = 2L, apply = 1L)
    expect_identical(si_snapshot(state), before)

    # Back to the inputs the search saw: the same candidates apply again.
    state$meta <- start
    session$flushReact()
    expect_null(stale())
    session$setInputs(results_rows_selected = 2L, apply = 2L)
    expect_identical(
      state$meta$item[state$meta$status == "parked" & state$meta$construct != "Speed"],
      drop_of(2)
    )
    expect_identical(state$meta$status[state$meta$construct == "Speed"], rep("parked", 3))
    expect_null(stale())

    # A second choice from the same search replaces the first: it is applied
    # to the pool the search ran on, not to what the first choice left.
    session$setInputs(results_rows_selected = two, apply = 3L)
    expect_setequal(
      state$meta$item[state$meta$status == "parked" & state$meta$construct != "Speed"],
      drop_of(two)
    )
    expect_identical(sum(state$meta$status == "active"), length(r$kept[[two]]))
    expect_null(stale())

    # The item data changed (an item reverse scored): refused.
    SIEVER:::sieve_apply_reverse(state, pool[1])
    session$flushReact()
    expect_false(is.null(stale()))
    before <- si_snapshot(state)
    session$setInputs(results_rows_selected = 1L, apply = 4L)
    expect_identical(si_snapshot(state), before)
    # Confirm the changed scoring before requesting another search.
    sieve_confirm_types(state)
    session$setInputs(run = 2)
    expect_null(stale())
    expect_identical(res()$revision, SIEVER:::sieve_revision(state))
  })
})

test_that("an optimizer result is discarded when another dataset is loaded, even an identical one", {
  shiny::testServer(si_server, {
    session$setInputs(`ingest-source` = "hs9", `ingest-basis` = "theoretical", `ingest-palette` = "nd")
    si_quiet(session$setInputs(`ingest-load` = 1L))
    # The selector takes effect only through the explicit basis action.
    session$setInputs(`ingest-basis` = "empirical", `ingest-apply_basis` = 1L)
    expect_identical(state$construct_basis, "empirical")
    speed <- state$meta$item[state$meta$construct == "Speed"]
    for (it in speed) {
      session$setInputs(`ingest-cell_edit` = list(item = it, field = "status", value = "Dropped"))
    }
    session$setInputs(
      `optimize-max_drop` = 1, `optimize-min_keep` = 2, `optimize-ss` = FALSE,
      `optimize-cap` = 200
    )
    si_quiet(session$setInputs(`optimize-run` = 1L))
    found <- state$codelog[grepl("Optimizer: searched 16 item-sets", state$codelog, fixed = TRUE)]
    expect_length(found, 1L)

    # The same file is loaded again: the same names, the same values, and
    # all nine items included. The candidate from the first dataset must not
    # be applied here and exclude items of this one.
    # Load asks first with a study in the session.
    si_quiet(session$setInputs(`ingest-load` = 2L))
    si_quiet(session$setInputs(`ingest-load_replace` = 1L))
    expect_identical(state$dataset_id, "D2")
    expect_identical(state$meta$status, rep("active", 9))
    before <- si_snapshot(state)
    session$setInputs(`optimize-results_rows_selected` = 2L, `optimize-apply` = 1L)
    expect_identical(si_snapshot(state), before)
    expect_identical(state$meta$status, rep("active", 9))
  })
})

# ---- The invariants, under a random sequence of the real actions -----------

test_that("any sequence of live reversals, bounds, exclusions and loads keeps the data, the ledger and the script consistent", {
  # Non-integer scores, so that subtraction alone would not restore them.
  fx <- sieve_hs9_fixture()
  scores <- fx$data
  scores[fx$item_cols] <- lapply(fx$data[fx$item_cols], function(v) v / 7 + 0.1)
  path <- file.path(tempdir(), "si_scores.csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(scores[, c("id", fx$item_cols)], path, row.names = FALSE)

  fresh <- NULL # the ledger as first built from the file
  consistent <- function(state, loaded, info) {
    m <- sieve_meta_keying(state$meta)
    expect_identical(state$raw, loaded, info = info) # never written
    for (i in seq_len(nrow(m))) {
      it <- m$item[i]
      x <- state$items[[it]]
      raw <- as.numeric(loaded[[it]])
      if (isTRUE(m$reverse[i])) {
        # One reflection of the values as loaded, within the stored bounds.
        expect_identical(x, (m$rev_lo[i] + m$rev_hi[i]) - raw, info = paste(info, it))
        expect_identical(m$note[i], "reverse coded", info = paste(info, it))
      } else {
        expect_identical(x, loaded[[it]], info = paste(info, it)) # the loaded values themselves
        expect_true(is.na(m$note[i]), info = paste(info, it))
      }
      # The summaries describe the current values (computed here without the
      # ledger's own expressions, so agreement is to rounding) ...
      expect_equal(c(m$mean[i], m$sd[i], m$min[i], m$max[i]),
        c(
          sum(x) / length(x), sqrt(sum((x - sum(x) / length(x))^2) / (length(x) - 1)),
          sort(x)[1], sort(x)[length(x)]
        ),
        tolerance = 1e-13, info = paste(info, it)
      )
      # ... and a row that is back at the loaded values is the loaded row, exactly.
      if (!isTRUE(m$reverse[i])) {
        expect_identical(m[i, c("mean", "sd", "min", "max", "n_missing", "n_levels", "type")],
          fresh[fresh$item == it, c("mean", "sd", "min", "max", "n_missing", "n_levels", "type")],
          info = paste(info, it)
        )
      }
    }
    # Every record belongs to a dataset that was loaded, and the last one
    # for the current dataset describes the current revision.
    mine <- Filter(function(r) identical(r$dataset_id, state$dataset_id), state$transforms)
    if (length(mine)) {
      last <- mine[[length(mine)]]
      expect_true(SIEVER:::sieve_same_revision(last$revision, SIEVER:::sieve_revision(state),
        what = c("dataset", "data")
      ), info = info)
    }
  }

  withr::local_seed(113)
  # Reversals are pressed in the Reverse? row above the grid, after the
  # Response bounds row has declared bounds for the same scope, or cleared
  # them, or neither; undo records with origin "prepare" are kept here by
  # the same rule and presented to the service the Reverse? cell and row
  # call. An
  # item's Response bounds cell is typed into as well. The rows act on a
  # scope, so the items are given the constructs and facets of the fixture
  # (typed into their cells after every load): the scopes then hold one to
  # nine included items.
  construct_of <- stats::setNames(
    rep(names(fx$theory_map), lengths(fx$theory_map)),
    unlist(fx$theory_map, use.names = FALSE)
  )
  facet_of <- c(
    t1_visual_perception = "V1", t2_cubes = "V2", t4_lozenges = "V2",
    t6_paragraph_comprehension = "T1", t7_sentence = "T1", t9_word_meaning = "T2",
    t10_addition = "S1", t12_counting_groups_of_dots = "S2", t13_straight_and_curved_capitals = "S2"
  )
  scopes <- c("all", paste0("construct:", names(fx$theory_map)), paste0("facet:", unique(facet_of)))
  scope_prob <- c(1, rep(1, length(fx$theory_map)), rep(1.5, length(unique(facet_of))))
  # The included items of a scope, read as the Ingest server reads it.
  in_scope <- function(m, scope) {
    act <- m$status == "active"
    kind <- sub(":.*$", "", scope)
    name <- sub("^[^:]*:", "", scope)
    switch(kind,
      construct = m$item[act & m$construct %in% name],
      facet = m$item[act & m$facet %in% name],
      m$item[act]
    )
  }
  edits <- 0L # every cell edit carries its own number, so that each is a new value
  cell_edit <- function(session, item, field, value) {
    edits <<- edits + 1L
    session$setInputs(`ingest-cell_edit` = list(item = item, field = field, value = value, n = edits))
  }
  organize <- function(session, state) {
    for (it in names(facet_of)) {
      cell_edit(session, it, "construct", construct_of[[it]])
      cell_edit(session, it, "facet", facet_of[[it]])
    }
    expect_identical(state$meta$construct, unname(construct_of[state$meta$item]))
    expect_identical(state$meta$facet, unname(facet_of[state$meta$item]))
  }
  keying_of <- function(m, it) {
    m <- sieve_meta_keying(m)
    i <- match(it, m$item)
    list(reverse = isTRUE(m$reverse[i]), lo = m$rev_lo[i], hi = m$rev_hi[i], bounds = m$rev_bounds[i])
  }

  shiny::testServer(si_server, {
    session$setInputs(`ingest-source` = "file", `ingest-basis` = "theoretical", `ingest-palette` = "nd")
    session$setInputs(`ingest-file` = list(name = "si_scores.csv", datapath = path))
    session$setInputs(`ingest-load` = 1L)
    loaded <- state$raw
    fresh <<- state$meta
    # The identifier column is numeric. Its role is proposed as identifier,
    # so it is not in the item pool, and an uploaded file waits
    # for the researcher to review the roles and confirm.
    items <- names(state$items)
    expect_identical(sort(items), sort(fx$item_cols))
    expect_identical(sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), "id")
    expect_false(isTRUE(state$confirmed))
    organize(session, state)
    session$setInputs(`ingest-confirm` = 1L)
    expect_true(isTRUE(state$confirmed))
    consistent(state, loaded, "after load")
    presses <- c(bounds_apply = 0L, bounds_clear = 0L, keying_apply = 0L, load = 1L)
    made <- c(cell = 0L, row = 0L, undone = 0L, other_dataset = 0L)
    # The records an undo is presented with, one per item: the net change
    # to each item, so that a reversal taken back by the next one leaves
    # nothing to undo. They are not dropped when another dataset is loaded,
    # so that the service's refusal of them is exercised too.
    undo_records <- list()
    prior_transforms <- list()
    for (step in 1:70) {
      # Two loads at fixed steps, so that more than one dataset is always
      # visited (and records of an earlier one are presented to an undo),
      # and the others at random.
      act <- if (step %in% c(30L, 55L)) {
        "load"
      } else {
        sample(c("grid", "bounds_cell", "row", "status", "load"), 1L,
          prob = c(4, 1.5, 6, 2, 0.6)
        )
      }
      info <- sprintf("step %d (%s)", step, act)
      known <- names(state$transforms)
      if (act == "grid") {
        it <- sample(items, 1L)
        on <- isTRUE(state$meta$reverse[state$meta$item == it])
        cell_edit(session, it, "reverse", if (on) "No" else "Yes")
        made[["cell"]] <- made[["cell"]] + length(setdiff(names(state$transforms), known))
      } else if (act == "bounds_cell") {
        it <- sample(items, 1L)
        typed <- sample(c("-3 to 60.5", "0 to 60", "", "1 to"), 1L)
        was <- keying_of(state$meta, it)
        held <- state$items
        cell_edit(session, it, "bounds", typed)
        now <- keying_of(state$meta, it)
        expect_identical(state$items, held, info = info) # declaring or clearing changes no value
        if (was$reverse || identical(typed, "1 to")) {
          # A reversed item keeps the bounds of its reversal; text that is
          # not two numbers declares nothing.
          expect_identical(now, was, info = info)
        } else if (nzchar(typed)) {
          bounds <- if (identical(typed, "0 to 60")) c(0, 60) else c(-3, 60.5)
          expect_identical(now, list(reverse = FALSE, lo = bounds[1], hi = bounds[2], bounds = "declared"), info = info)
        } else if (identical(was$bounds, "declared")) {
          expect_identical(now, list(reverse = FALSE, lo = NA_real_, hi = NA_real_, bounds = NA_character_), info = info)
        } else {
          expect_identical(now, was, info = info) # only a declaration is cleared
        }
      } else if (act == "row") {
        scope <- sample(scopes, 1L, prob = scope_prob)
        bounds <- sample(c("declare", "clear", "none"), 1L, prob = c(2, 1, 2))
        if (bounds == "declare") {
          session$setInputs(`ingest-bounds_lo` = -3, `ingest-bounds_hi` = 60.5, `ingest-types_scope` = scope)
          presses[["bounds_apply"]] <- presses[["bounds_apply"]] + 1L
          session$setInputs(`ingest-bounds_apply` = presses[["bounds_apply"]])
        } else if (bounds == "clear") {
          session$setInputs(`ingest-types_scope` = scope)
          presses[["bounds_clear"]] <- presses[["bounds_clear"]] + 1L
          session$setInputs(`ingest-bounds_clear` = presses[["bounds_clear"]])
        }
        if (bounds != "none") {
          # The bounds row writes the ledger alone.
          expect_identical(names(state$transforms), known, info = info)
        }
        want <- sample(c("Yes", "No"), 1L, prob = c(2, 1))
        sc <- in_scope(state$meta, scope)
        session$setInputs(`ingest-types_scope` = scope, `ingest-keying_to` = want)
        presses[["keying_apply"]] <- presses[["keying_apply"]] + 1L
        session$setInputs(`ingest-keying_apply` = presses[["keying_apply"]])
        new <- state$transforms[setdiff(names(state$transforms), known)]
        made[["row"]] <- made[["row"]] + length(new)
        # Every included item of the scope now has the Reverse? chosen, and
        # no other item was touched.
        expect_identical(state$meta$reverse[match(sc, state$meta$item)] %in% TRUE, rep(want == "Yes", length(sc)), info = info)
        expect_true(all(vapply(new, function(r) r$item, character(1)) %in% sc), info = info)
        if (length(new)) {
          expect_false(isTRUE(state$confirmed), info = info) # the row withdraws the confirmation
        }
        for (rec in new) {
          prior <- undo_records[[rec$item]]
          if (!is.null(prior) && identical(prior$dataset_id, rec$dataset_id) &&
            identical(prior$from, rec$to) && identical(prior$before_key, rec$after_key)) {
            undo_records[[rec$item]] <- NULL
          } else {
            undo_records[[rec$item]] <- rec
          }
        }
      } else if (act == "status") {
        it <- sample(items, 1L)
        on <- state$meta$status[state$meta$item == it] == "active"
        if (!on || sum(state$meta$status == "active") > 4L) {
          cell_edit(session, it, "status", if (on) "Dropped" else "In use")
        }
      } else {
        prior_transforms <- c(prior_transforms, state$transforms)
        presses[["load"]] <- presses[["load"]] + 1L
        session$setInputs(`ingest-load` = presses[["load"]])
        # With a study in the session, Load asks first (the dialog's Load and replace).
        session$setInputs(`ingest-load_replace` = presses[["load"]] - 1L)
        organize(session, state)
        session$setInputs(`ingest-confirm` = presses[["load"]])
        expect_identical(state$dataset_id, sprintf("D%d", presses[["load"]]), info = info)
        expect_length(Filter(function(r) identical(r$dataset_id, state$dataset_id), state$transforms), 0L)
        # The records kept in the dataset just replaced, presented to the
        # undo, are every one refused, and nothing changes. They stay kept,
        # so a later undo meets them again.

      }
      consistent(state, loaded, info)
    }
    # The sequence did exercise the service: reversals from the Reverse?
    # cell and the Reverse? row (origin "ingest" both), undos of kept
    # records through the service (origin "prepare"), and more than one
    # dataset.
    all_transforms <- c(prior_transforms, state$transforms)
    origins <- vapply(all_transforms, function(r) r$origin, character(1))
    undone <- vapply(all_transforms, function(r) !is.null(r$undo_of), logical(1))
    expect_gt(made[["cell"]], 5L)
    expect_gt(made[["row"]], 5L)
    expect_identical(sum(origins == "ingest"), made[["cell"]] + made[["row"]])
    expect_gt(presses[["load"]], 1L)
    expect_gt(presses[["bounds_apply"]], 0L)
    expect_gt(presses[["bounds_clear"]], 0L)
    expect_gt(sum(vapply(all_transforms, function(r) identical(r$bounds, "declared"), logical(1))), 0L)
    expect_gt(sum(vapply(all_transforms, function(r) identical(r$applied, FALSE), logical(1))), 3L)
  })
})

test_that("the session script replays a random sequence of reversals to the data the app holds", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9")
    session$setInputs(load = 1)
  })
  fx <- sieve_hs9_fixture()
  withr::local_seed(113)
  for (step in 1:25) {
    it <- sample(fx$item_cols, 1L)
    declared <- sample(c(TRUE, FALSE), 1L)
    shiny::isolate(SIEVER:::sieve_apply_reverse(
      st, it,
      lo = if (declared) 0 else NULL, hi = if (declared) 400 else NULL,
      origin = sample(c("ingest", "ai"), 1L)
    ))
    if (step %in% c(8, 16)) {
      shiny::isolate(sieve_log(
        st, sprintf("Read the item data at step %d", step),
        sprintf("seen_%d <- lapply(X, as.numeric)", step)
      ))
      assign(sprintf("held_%d", step), lapply(shiny::isolate(st$items), as.numeric))
    }
  }
  txt <- NULL
  shiny::testServer(mod_repro_server, args = list(state = st), {
    txt <<- script()
  })
  env <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = txt), envir = env))
  # The responses are integers, so the replayed subtractions are exact and
  # the script ends on the very values the app holds, and passed through the
  # same values on the way.
  expect_identical(lapply(env$items, as.numeric), lapply(shiny::isolate(st$items), as.numeric))
  expect_identical(env$seen_8, held_8)
  expect_identical(env$seen_16, held_16)
  expect_identical(SIEVER:::sieve_data_key(env$items), shiny::isolate(SIEVER:::sieve_revision(st))$data_key)
  expect_gt(sum(shiny::isolate(st$meta$reverse)), 0L)
})

# ---- The recorded R of the Ingest tab, replayed in a clean R process --------

# Replay the session script in a new R process (Rscript --vanilla, with the
# source under test first on the library path) and return the objects it
# ends with, or the reason it stopped. A new process is the point: nothing
# of the session is at hand there except what the script itself defines.
si_replay <- function(state, keep = c("items", "meta")) {
  held <- new.env(parent = emptyenv())
  shiny::testServer(mod_repro_server, args = list(state = state), {
    held$code <- script()
  })
  out <- tempfile(fileext = ".rds")
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    # The source under test, installed first on the path (helper-interface.R):
    # the script checks the SIEVER version and calls SIEVER:: functions.
    sprintf(".libPaths(%s)", paste(deparse(compat_source_libs()), collapse = "")),
    held$code,
    sprintf(
      "saveRDS(mget(intersect(c(%s), ls())), %s)",
      paste(deparse(keep), collapse = ""), deparse(out)
    )
  ), script)
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(log, "status")
  list(
    ok = is.null(status) || identical(status, 0L), log = log, script = held$code,
    objects = if (file.exists(out)) readRDS(out) else NULL
  )
}

# The bundled example loaded by the real load observer, and a sequence of
# edits made in the real Ingest grid.
si_hs9_loaded <- function() {
  s <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = s), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    si_quiet(session$setInputs(load = 1L))
  })
  s
}
si_grid <- function(state, edits) {
  shiny::testServer(mod_ingest_server, args = list(state = state), {
    for (k in seq_along(edits)) {
      session$setInputs(cell_edit = c(edits[[k]], list(n = k)))
    }
  })
  invisible(state)
}
# The lavaan Holzinger and Swineford data, whose scores are not whole
# numbers, assembled as the other tests assemble a hand-built state, with the
# load recorded so that the script reads the same data.
si_lavaan_loaded <- function() {
  d <- lavaan::HolzingerSwineford1939
  its <- paste0("x", 1:9)
  fixture <- list(data = d, item_cols = its,
    theory_map = list(Visual = its[1:3], Textual = its[4:6], Speed = its[7:9]),
    name = "lavaan HS", labels = NULL, load_code = character())
  s <- siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) s$roles <- NULL)
  sieve_log(s, "Load data", "d <- lavaan::HolzingerSwineford1939")
  s
}

test_that("an undo step restores the values as read exactly, by row name in a view with fewer rows, and stops without its copy", {
  x <- withr::with_seed(113, stats::runif(60, 0.3, 9.7))
  d <- data.frame(id = seq_along(x), v = x, w = rev(x))
  m <- sieve_build_meta(d, c("v", "w"))
  once <- SIEVER:::sieve_reverse_item(d[, c("v", "w")], m, "v", loaded = d$v)
  back <- SIEVER:::sieve_reverse_item(once$data, once$meta, "v", loaded = d$v)
  expect_identical(once$record$applied, TRUE)
  expect_identical(back$record$applied, FALSE)
  # Subtraction alone would not give x back; the recorded steps do.
  s <- min(x) + max(x)
  expect_false(identical(s - (s - x), x))
  env <- new.env(parent = globalenv())
  env$items <- d[, c("v", "w")]
  env$inv_data <- env$items[seq(1, 60, by = 2), ] # a view with fewer rows
  eval(parse(text = c(once$code, back$code)), envir = env)
  expect_identical(env$items$v, x)
  expect_identical(env$items, d[, c("v", "w")])
  expect_identical(env$inv_data$v, x[seq(1, 60, by = 2)])
  expect_identical(env$values_as_read$v, x)
  # The data as read are not read by either step: a script that binds `d` to
  # the analytic item data in between (the calibration and validation split
  # does) still ends on the values as read.
  rebound <- new.env(parent = globalenv())
  rebound$items <- d[, c("v", "w")]
  eval(parse(text = c(once$code, "d <- items", back$code)), envir = rebound)
  expect_identical(rebound$items$v, x)
  expect_identical(rebound$d$v, s - x)
  # Without the kept copy the undo step stops and says so, rather than
  # subtracting a second time.
  bare <- new.env(parent = globalenv())
  bare$items <- once$data
  expect_error(eval(parse(text = back$code), envir = bare),
    "kept aside as values_as_read",
    fixed = TRUE
  )
  expect_identical(bare$items, once$data)
  # A step that cannot find the item data stops as well.
  broken <- new.env(parent = globalenv())
  broken$items <- c("v", "w")
  expect_error(eval(parse(text = once$code), envir = broken),
    "`items` is not a data frame that holds it",
    fixed = TRUE
  )
  expect_identical(broken$items, c("v", "w"))
})

test_that("the Ingest tab's recorded R replays the item data in a clean R process: reversal, undo, drop then reversal, reversal then drop", {
  skip_on_cran()
  fx <- sieve_hs9_fixture()
  it1 <- fx$item_cols[1]
  it2 <- fx$item_cols[2]
  same_items <- function(r, s, info) {
    expect_true(r$ok, info = paste(info, paste(utils::tail(r$log, 5), collapse = " | ")))
    ri <- r$objects$items
    expect_true(is.data.frame(ri), info = info)
    expect_identical(lapply(ri, as.numeric), lapply(shiny::isolate(s$items), as.numeric), info = info)
    expect_identical(SIEVER:::sieve_data_key(ri), shiny::isolate(SIEVER:::sieve_revision(s))$data_key, info = info)
  }
  edit <- function(item, field, value) list(item = item, field = field, value = value)

  # A reversal.
  s <- si_hs9_loaded()
  si_grid(s, list(edit(it1, "reverse", "Yes")))
  same_items(si_replay(s), s, "reversal")

  # A drop in the grid, then a reversal, after an analysis step has read the
  # item data (so the preamble sits before the drop).
  s <- si_hs9_loaded()
  shiny::isolate(sieve_log(s, "An analysis step", "n_before <- ncol(X)"))
  si_grid(s, list(edit(it2, "status", "Dropped"), edit(it1, "reverse", "Yes")))
  r <- si_replay(s)
  same_items(r, s, "drop then reversal")
  expect_identical(r$objects$meta$status[r$objects$meta$item == it2], "parked")
  expect_false(any(grepl("items <- setdiff(items", r$script, fixed = TRUE)))
  expect_true(any(grepl(sprintf('meta$status[meta$item %%in%% c("%s")] <- "parked"', it2), r$script, fixed = TRUE)))

  # A reversal, then a drop.
  s <- si_hs9_loaded()
  shiny::isolate(sieve_log(s, "An analysis step", "n_before <- ncol(X)"))
  si_grid(s, list(edit(it1, "reverse", "Yes"), edit(it2, "status", "Dropped")))
  same_items(si_replay(s), s, "reversal then drop")

  # A drop and the item brought back.
  s <- si_hs9_loaded()
  si_grid(s, list(edit(it2, "status", "Dropped"), edit(it1, "reverse", "Yes"), edit(it2, "status", "In use")))
  r <- si_replay(s)
  same_items(r, s, "drop, reversal, include again")
  expect_identical(r$objects$meta$status[r$objects$meta$item == it2], "active")

  # A reversal of non-integer scores, taken back: the script ends on the
  # values as loaded exactly, as the app does.
  d <- lavaan::HolzingerSwineford1939
  s <- si_lavaan_loaded()
  si_grid(s, list(edit("x7", "reverse", "Yes"), edit("x7", "reverse", "No")))
  expect_identical(shiny::isolate(s$items$x7), d$x7)
  r <- si_replay(s)
  same_items(r, s, "undo of a non-integer item")
  expect_identical(r$objects$items$x7, d$x7)

  # The same, with a logged step between them that binds `d` to the analytic
  # item data, as the calibration and validation split does.
  s <- si_lavaan_loaded()
  si_grid(s, list(edit("x7", "reverse", "Yes")))
  shiny::isolate(sieve_log(
    s, "Finalize: calibration/validation split",
    c(
      "set.seed(1234)",
      "d <- items[stats::complete.cases(items), active_item_names]",
      "val_idx <- sort(sample.int(nrow(d), max(1L, round(nrow(d) * 0.5))))"
    )
  ))
  si_grid(s, list(edit("x7", "reverse", "No")))
  r <- si_replay(s)
  same_items(r, s, "undo after a step that rebinds d")
  expect_identical(r$objects$items$x7, d$x7)

  # A drop logged the way the other tabs log it (a reassignment of
  # `items`), after an analysis step, then a reversal: the replay stops and
  # says why, instead of running on with a list where the item data were.
  s <- si_hs9_loaded()
  shiny::isolate({
    sieve_log(s, "An analysis step", "n_before <- ncol(X)")
    s$meta$status[s$meta$item == it2] <- "parked"
    sieve_log(s, sprintf("Exclude %s", it2), sprintf('items <- setdiff(items, "%s")', it2))
  })
  si_grid(s, list(edit(it1, "reverse", "Yes")))
  r <- si_replay(s)
  expect_false(r$ok)
  expect_true(any(grepl("replayed item data do not match", r$log, fixed = TRUE)))
  expect_null(r$objects)
})

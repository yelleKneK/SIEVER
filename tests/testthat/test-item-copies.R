# The same item twice: an item and its reversed copy, both in the data.

copies_fixture <- function() {
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols]
  x <- d$t2_cubes
  lo <- min(x, na.rm = TRUE)
  hi <- max(x, na.rm = TRUE)
  d$t2_cubes_r <- (lo + hi) - x
  by_item <- stats::setNames(
    rep(names(fx$theory_map), lengths(fx$theory_map)),
    unlist(fx$theory_map, use.names = FALSE)
  )
  construct <- c(unname(by_item[fx$item_cols]), "Visual")
  meta <- data.frame(
    item = names(d), status = "active", construct = construct,
    reverse = FALSE, stringsAsFactors = FALSE
  )
  list(d = d, meta = meta, sum = lo + hi)
}

test_that("an item and its reversed copy are found, and the one keyed with its construct is kept", {
  f <- copies_fixture()
  cp <- sieve_item_copies(f$d, f$meta)
  expect_identical(nrow(cp), 1L)
  expect_identical(c(cp$a, cp$b), c("t2_cubes", "t2_cubes_r"))
  expect_equal(cp$r, -1)
  expect_true(cp$constant_sum)
  expect_equal(cp$sum, f$sum)
  # Visual's other included items (t1, t4) run with t2_cubes, not its copy.
  rest <- rowMeans(f$d[, c("t1_visual_perception", "t4_lozenges")], na.rm = TRUE)
  expect_equal(cp$r_a, stats::cor(f$d$t2_cubes, rest, use = "pairwise.complete.obs"))
  expect_equal(cp$r_b, -cp$r_a)
  expect_gt(cp$r_a, 0)
  expect_identical(cp$keep, "t2_cubes")
  expect_identical(cp$drop, "t2_cubes_r")
})

test_that("the copy is judged the other way round when it is the one keyed with the construct", {
  f <- copies_fixture()
  # The original keyed against its construct: reflect it, so its copy is
  # the one that runs with Visual.
  original <- f$d$t2_cubes
  f$d$t2_cubes <- f$sum - original
  f$d$t2_cubes_r <- original
  cp <- sieve_item_copies(f$d, f$meta)
  expect_identical(cp$keep, "t2_cubes_r")
  expect_identical(cp$drop, "t2_cubes")
})

test_that("without a construct to read the direction from, no item is proposed", {
  f <- copies_fixture()
  f$meta$construct <- "Unassigned"
  cp <- sieve_item_copies(f$d, f$meta)
  expect_identical(nrow(cp), 1L)
  expect_true(is.na(cp$keep))
  expect_true(is.na(cp$drop))
  expect_true(is.na(cp$r_a))
})

test_that("a copy reverse scored here is the same item twice at +1, and the item not reversed is kept", {
  f <- copies_fixture()
  f$d$t2_cubes_r <- f$sum - f$d$t2_cubes_r
  f$meta$reverse[f$meta$item == "t2_cubes_r"] <- TRUE
  cp <- sieve_item_copies(f$d, f$meta)
  expect_equal(cp$r, 1)
  expect_false(cp$constant_sum)
  expect_identical(cp$keep, "t2_cubes")
  expect_identical(cp$drop, "t2_cubes_r")
})

test_that("distinct items, an excluded copy, and too few cases are not reported", {
  f <- copies_fixture()
  expect_identical(nrow(sieve_item_copies(f$d[, setdiff(names(f$d), "t2_cubes_r")], f$meta)), 0L)
  m <- f$meta
  m$status[m$item == "t2_cubes_r"] <- "parked"
  expect_identical(nrow(sieve_item_copies(f$d, m)), 0L)
  few <- f$d
  few$t2_cubes_r[-(1:9)] <- NA
  expect_identical(nrow(sieve_item_copies(few, f$meta)), 0L)
  expect_identical(nrow(sieve_item_copies(NULL, f$meta)), 0L)
})

test_that("an item saved three times is one set, and every copy is left out of the construct mean the keep rule reads", {
  f <- copies_fixture()
  f$d$t2_cubes_r2 <- f$d$t2_cubes_r
  f$meta <- rbind(f$meta, data.frame(item = "t2_cubes_r2", status = "active", construct = "Visual", reverse = FALSE))
  cp <- sieve_item_copies(f$d, f$meta)
  expect_identical(nrow(cp), 3L)
  rest <- rowMeans(f$d[, c("t1_visual_perception", "t4_lozenges")], na.rm = TRUE)
  expected <- stats::cor(f$d$t2_cubes, rest, use = "pairwise.complete.obs")
  pair <- cp[cp$a == "t2_cubes" & cp$b == "t2_cubes_r", ]
  expect_equal(pair$r_a, expected)
  expect_identical(pair$keep, "t2_cubes")
  # The two reversed copies are the same item twice, at +1.
  twin <- cp[cp$a == "t2_cubes_r" & cp$b == "t2_cubes_r2", ]
  expect_equal(twin$r, 1)
})

test_that("Screening says when a pair is an item and its reversed copy, read as the item grid reads it", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  x <- d$t2_cubes
  d$t2_cubes_r <- (min(x, na.rm = TRUE) + max(x, na.rm = TRUE)) - x
  items <- c(fx$item_cols, "t2_cubes_r")
  map <- fx$theory_map
  map$Visual <- c(map$Visual, "t2_cubes_r")
  st <- sieve_new_state()
  st$dataset_name <- "with a copy"
  st$raw <- d
  st$items <- d[, items, drop = FALSE]
  st$id_vars <- fx$id_vars
  st$meta <- sieve_build_meta(d, items, map, labels = c(fx$labels, t2_cubes_r = "t2_cubes_r"))
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    note <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(output$cor_singular)), collapse = " ")))
    expect_match(note, "An item and its reversed copy?", fixed = TRUE)
    expect_match(note, "t2_cubes and t2_cubes_r correlate near -1 (Pearson)", fixed = TRUE)
    expect_match(note, "The item grid on the Ingest tab names it and excludes the other in one click.", fixed = TRUE)
    # The largest correlations are named without a cutoff (not only pairs
    # with |r| > .85, and not as "Highly redundant pairs").
    expect_match(note, "The largest correlations between two items: t2_cubes", fixed = TRUE)
    expect_false(grepl("Highly redundant|> .85", note))
  })
})

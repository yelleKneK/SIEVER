# The modification index panel of the CFA page and the candidates lavaan's
# modindices() leaves without an index.
#
# On the bundled nine Holzinger and Swineford tests at their raw DMAR scale
# (standard deviations from 3.5 to 36), lavaan::modindices() returns no row
# for the residual covariance of speeded addition with speeded counting,
# although the same model on standardized items lists it first among the
# residual covariances at 34.145 (lavaan's own tutorial shows x7 ~~ x8 at
# 34.1), and a panel that showed its table alone would inherit the gap. The
# cause is in modindices(): it divides each candidate's squared score by its
# information given the model and sets the index NA where that information
# falls under a fixed absolute tolerance, .Machine$double.eps^(1/3), then
# removes the NA rows by default. The information of a residual covariance
# scales with the reciprocal of the product of the two variances (410 to 1320
# here), so the tolerance is crossed although the index itself is invariant to
# the scale of the items. sieve_modification_indices() asks lavaan for the
# full table, computes every candidate lavaan nulled by the equivalent
# univariate score test on the same fit (lavaan::lavTestScore(), which inverts
# the information in full and is not subject to the tolerance), names what it
# recovered and what it could not, and the page says so above the table.
# Tested here: the pair is listed for the nine tests with the value of the
# standardized model and of the score test, its EPC in the items' units, the
# rows lavaan computes untouched, nothing recovered where lavaan is complete,
# the five pairs of the 24 tests, parameters fixed by identification never
# treated as candidates, and the page's table and note.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

mi_nine <- function() {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  meta$type <- "continuous"
  list(fx = fx, items = fx$data[, fx$item_cols, drop = FALSE], meta = meta)
}

mi_state <- function() {
  siever_test_study(configure = function(s) {
    # This test isolates modification indices for the nine-indicator CFA.
    roles <- shiny::isolate(s$roles)
    roles$role[roles$role == "auxiliary"] <- "identifier"
    s$roles <- roles
  })
}

mi_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}

pair <- "t10_addition ~~ t12_counting_groups_of_dots"
speed_pairs <- c(
  pair,
  "t10_addition ~~ t13_straight_and_curved_capitals",
  "t12_counting_groups_of_dots ~~ t13_straight_and_curved_capitals"
)

test_that("the speeded addition with speeded counting pair is listed for the nine tests at their raw scale", {
  hs <- mi_nine()
  fit <- sieve_fit_cfa(hs$items, hs$meta)
  # lavaan's own table: the row is there without an index (the gap).
  direct <- lavaan::modindices(fit, na_remove = FALSE)
  at <- direct$op == "~~" & direct$lhs == "t10_addition" &
    direct$rhs == "t12_counting_groups_of_dots"
  expect_identical(sum(at), 1L)
  expect_true(is.na(direct$mi[at]))
  expect_false(pair %in% with(lavaan::modindices(fit), paste(lhs, op, rhs)))

  mi <- sieve_modification_indices(fit, top = 100L)
  expect_true(pair %in% mi$table$Path)
  row <- mi$table[mi$table$Path == pair, ]
  expect_identical(row$Type, "correlated residual")
  # The value of the same model on standardized items (the index and the
  # fully standardized EPC do not depend on the scale of the items; the
  # EPC is equivariant, in the items' own units).
  z <- lavaan::modindices(
    sieve_fit_cfa(as.data.frame(scale(hs$items)), hs$meta), na_remove = FALSE
  )
  zrow <- z[z$op == "~~" & z$lhs == "t10_addition" & z$rhs == "t12_counting_groups_of_dots", ]
  expect_equal(row$MI, zrow$mi, tolerance = 1e-4)
  expect_equal(row$MI, 34.145, tolerance = 1e-4)
  expect_equal(row$Std.EPC, zrow$sepc.all, tolerance = 1e-4)
  expect_equal(
    row$EPC,
    zrow$epc * stats::sd(hs$items$t10_addition) * stats::sd(hs$items$t12_counting_groups_of_dots),
    tolerance = 1e-4
  )
  # The value of the score test on the fit itself, to the last digit.
  ts <- lavaan::lavTestScore(fit, add = pair, epc = TRUE, standardized = TRUE)
  expect_equal(row$MI, ts$uni$X2[1L], tolerance = 1e-12)
  # It ranks second, after the cross loading of the capitals test on Visual,
  # as in lavaan's tutorial.
  expect_identical(
    mi$table$Path[1:2],
    c("Visual =~ t13_straight_and_curved_capitals", pair)
  )
  # The three within-Speed residual covariances are the recovered rows;
  # nothing was dropped; the panel's text names them.
  expect_setequal(mi$recovered, speed_pairs)
  expect_identical(mi$dropped, character(0))
  said <- sieve_mi_recovery_text(mi)
  expect_length(said, 1L)
  expect_match(said, "3 candidate paths without an index", fixed = TRUE)
  expect_match(said, pair, fixed = TRUE)
  expect_match(said, "lavaan::lavTestScore()", fixed = TRUE)
  # The rows lavaan computes are lavaan's, unchanged.
  computed <- direct[!is.na(direct$mi) & direct$mi >= 3.84, ]
  at <- match(paste(computed$lhs, computed$op, computed$rhs), mi$table$Path)
  expect_false(anyNA(at))
  expect_equal(mi$table$MI[at], computed$mi, tolerance = 1e-12)
  expect_equal(mi$table$EPC[at], computed$epc, tolerance = 1e-12)
  expect_equal(mi$table$Std.EPC[at], computed$sepc.all, tolerance = 1e-12)
  # Sorted, and only at or above the minimum.
  expect_false(is.unsorted(rev(mi$table$MI)))
  expect_true(all(mi$table$MI >= 3.84))
  # Latent variances fixed at 1 and latent intercepts are not candidates
  # and are never named.
  expect_false(any(grepl("^(Visual|Textual|Speed) ~~ (Visual|Textual|Speed)$", c(mi$recovered, mi$dropped))))
  expect_false(any(grepl("~1", c(mi$recovered, mi$dropped, mi$table$Path), fixed = TRUE)))
})

test_that("where lavaan computes every candidate nothing is recovered and the table is lavaan's", {
  hs <- mi_nine()
  fit <- sieve_fit_cfa(as.data.frame(scale(hs$items)), hs$meta)
  mi <- sieve_modification_indices(fit)
  direct <- lavaan::modindices(fit, sort = TRUE, minimum_value = 3.84)
  expect_identical(mi$recovered, character(0))
  expect_identical(mi$dropped, character(0))
  expect_null(sieve_mi_recovery_text(mi))
  expect_equal(mi$table$MI, utils::head(direct$mi, 25L), tolerance = 1e-12)
  expect_true(pair %in% mi$table$Path)
  expect_match(sieve_modification_indices(fit, minimum = 1e6)$note, "No modification indices", fixed = TRUE)
})

test_that("the 24 tests: every pair lavaan leaves without an index is recovered, each equal to its score test", {
  fx <- sieve_hs_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  meta$type <- "continuous"
  items <- fx$data[, fx$item_cols, drop = FALSE]
  fit <- sieve_fit_cfa(items, meta)
  direct <- lavaan::modindices(fit, na_remove = FALSE)
  nulled <- direct[is.na(direct$mi) & direct$op == "~~" & direct$lhs != direct$rhs &
    direct$lhs %in% fx$item_cols & direct$rhs %in% fx$item_cols, ]
  expect_gt(nrow(nulled), 0L)
  mi <- sieve_modification_indices(fit, top = 1000L, minimum = 0)
  expect_setequal(mi$recovered, paste(nulled$lhs, nulled$op, nulled$rhs))
  expect_identical(mi$dropped, character(0))
  for (i in seq_len(nrow(nulled))) {
    term <- paste(nulled$lhs[i], nulled$op[i], nulled$rhs[i])
    ts <- lavaan::lavTestScore(fit, add = term)
    expect_equal(mi$table$MI[mi$table$Path == term], ts$uni$X2[1L], tolerance = 1e-12)
  }
  # And each agrees with the same model on standardized items, to the
  # optimizer's tolerance (an absolute bound: one of the five is near zero).
  z <- lavaan::modindices(sieve_fit_cfa(as.data.frame(scale(items)), meta), na_remove = FALSE)
  zkey <- paste(z$lhs, z$op, z$rhs)
  for (term in mi$recovered) {
    expect_lt(abs(mi$table$MI[mi$table$Path == term] - z$mi[zkey == term]), 1e-3)
  }
})

test_that("parameters fixed by identification are not candidates under either identification", {
  hs <- mi_nine()
  marker <- sieve_fit_cfa(hs$items, hs$meta, std_lv = FALSE)
  direct <- lavaan::modindices(marker, na_remove = FALSE)
  # lavaan lists the marker loadings without an index; they are not
  # candidates and are neither recovered nor reported as dropped.
  markers <- direct[direct$op == "=~" & is.na(direct$mi), ]
  expect_gt(nrow(markers), 0L)
  mi <- sieve_modification_indices(marker, top = 100L)
  named <- c(mi$recovered, mi$dropped, mi$table$Path)
  expect_false(any(paste(markers$lhs, markers$op, markers$rhs) %in% named))
  expect_setequal(mi$recovered, speed_pairs)
  expect_identical(mi$dropped, character(0))
  # The candidate rule itself.
  cand <- sieve_mi_candidate(direct, marker)
  expect_true(all(direct$op[cand] %in% c("=~", "~~")))
  expect_false(any(cand[direct$op == "=~" & is.na(direct$mi)]))
  expect_true(all(cand[direct$op == "~~" & direct$lhs != direct$rhs &
    direct$lhs %in% hs$fx$item_cols & direct$rhs %in% hs$fx$item_cols]))
})

test_that("the CFA page lists the pair and says where its index came from", {
  st <- mi_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    expect_identical(cfa_run()$status$code, "ok")
    # The table is served to the browser by DataTables' server side, so the
    # rendered payload holds the container and not the rows; the rows are
    # read from the result the page keeps for it.
    expect_no_error(output$mi_tbl)
    header <- jsonlite::fromJSON(as.character(unclass(output$mi_tbl)[1L]), simplifyVector = FALSE)$x$container
    expect_match(header, "Suggested path", fixed = TRUE)
    shown <- mi_result()
    expect_true(pair %in% shown$table$Path)
    expect_identical(shown$table$Path[1:2], c("Visual =~ t13_straight_and_curved_capitals", pair))
    expect_equal(shown$table$MI[2L], 34.145, tolerance = 1e-4)
    expect_setequal(shown$recovered, speed_pairs)
    note <- mi_text(output$mi_note)
    expect_match(note, "3 candidate paths without an index", fixed = TRUE)
    expect_match(note, pair, fixed = TRUE)
    expect_match(note, "lavaan::lavTestScore()", fixed = TRUE)
    # The same held result serves both outputs: a second read computes nothing new.
    first <- mi_result()
    expect_identical(mi_result(), first)
  })
})

test_that("grouped modification indices name their group and keep each group's index", {
  hs <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9", data = hs, group = "school",
    group.equal = c("loadings", "intercepts"), std.lv = TRUE)
  mi <- sieve_modification_indices(fit, minimum = 0, top = 1000L)
  # Each row names its group: without it "visual =~ x5" would appear once,
  # without saying whose it is (Pasteur's index for the same path differs
  # from Grant-White's).
  expect_identical(names(mi$table), c("Group", "Path", "Type", "MI", "EPC", "Std.EPC"))
  direct <- suppressWarnings(lavaan::modindices(fit, sort = FALSE, na_remove = TRUE))
  labels <- lavaan::lavInspect(fit, "group.label")
  key <- paste(direct$lhs, direct$op, direct$rhs, labels[direct$group])
  at <- match(paste(mi$table$Path, mi$table$Group), key)
  expect_false(anyNA(at))
  expect_equal(mi$table$MI, direct$mi[at], tolerance = 1e-12)
  x5 <- mi$table[mi$table$Path == "visual =~ x5", ]
  expect_setequal(x5$Group, c("Pasteur", "Grant-White"))
  expect_gt(abs(diff(x5$MI)), .1)
  # lavaan's equality-constraint warning is passed through once.
  expect_true(any(grepl("lavTestScore", mi$warnings, fixed = TRUE)))
})

test_that("modification indices type regressions, latent covariances, and means by what they add", {
  hs <- lavaan::HolzingerSwineford1939
  sem <- lavaan::sem("visual =~ x1+x2+x3; textual =~ x4+x5+x6; speed =~ x7+x8+x9
    textual ~ visual; speed ~ textual", data = hs, std.lv = TRUE)
  mi <- sieve_modification_indices(sem, top = 1000L)
  type <- stats::setNames(mi$table$Type, mi$table$Path)
  # A row not "=~" is typed by what it adds, not called a "correlated
  # residual" unless it is one.
  expect_identical(unname(type["textual ~ speed"]), "regression path")
  expect_identical(unname(type["visual ~~ speed"]), "latent residual covariance")
  expect_identical(unname(type["visual =~ x9"]), "cross loading")
  expect_identical(unname(type["x7 ~~ x8"]), "correlated residual")
  orthogonal <- lavaan::cfa("visual =~ x1+x2+x3; textual =~ x4+x5+x6", data = hs,
    std.lv = TRUE, orthogonal = TRUE)
  ortho <- sieve_modification_indices(orthogonal, top = 1000L)
  expect_identical(ortho$table$Type[ortho$table$Path == "visual ~~ textual"], "latent covariance")
  rows <- data.frame(
    lhs = c("visual", "x1", "x1", "visual", "speed"), op = c("~1", "~1", "~~", "~~", "~~"),
    rhs = c("", "", "x1", "x1", "textual"), stringsAsFactors = FALSE
  )
  expect_identical(sieve_mi_type(rows, sem), c(
    "latent mean", "intercept", "variance", "observed and latent covariance", "latent residual covariance"
  ))
})

test_that("multilevel modification indices name the level of every path", {
  fit <- lavaan::sem("level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1
  level: 2
    fb =~ y1 + y2 + y3", data = lavaan::Demo.twolevel, cluster = "cluster")
  mi <- sieve_modification_indices(fit, minimum = 0, top = 1000L)
  expect_identical(names(mi$table), c("Level", "Path", "Type", "MI", "EPC", "Std.EPC"))
  expect_true(all(mi$table$Level == "within"))
  # The between-level residual covariances have no index; the dropped list
  # names the level of each and lists each path once.
  expect_identical(mi$dropped, c("y1 ~~ y2 (level: cluster)", "y1 ~~ y3 (level: cluster)", "y2 ~~ y3 (level: cluster)"))
  expect_false(anyDuplicated(mi$dropped) > 0L)
})

# The item set search contract: the candidates and their count, held
# constructs and infeasible constraints, the capacity limit, and the labels
# of the statistics. The Optimizer's messages say items are excluded, not
# dropped.
#
# What is asserted here, and against what:
#
# * the candidates the search builds are compared with a brute-force listing
#   of every subset of the included items, filtered by the policy written
#   out in words below (it shares no code with the package);
# * the algebraic count is compared with the size of that listing;
# * the constraint checker is compared with the same independent predicate
#   on every subset, the ones that violate a constraint included;
# * the statistics of the current item set are compared with a closed form
#   (omega of three indicators), with a direct lavaan fit, and with a direct
#   psych analysis, and their sample sizes with counted rows;
# * whether the one-factor solution behind an omega is improper is decided
#   from the closed form residual variances of three indicators and from a
#   direct lavaan fit, not from the flag the package reads;
# * the counts for smaller settings in a refusal are compared with brute
#   force, and the binomial coefficients with Pascal's triangle.
#
# Author: Ken Kelley

skip_if_not_installed("shiny")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# A small item ledger. Items are named i01, i02, ... in ledger order.
ledger <- function(construct, facet = NA_character_, status = "active") {
  n <- length(construct)
  data.frame(
    item = sprintf("i%02d", seq_len(n)),
    label = sprintf("i%02d", seq_len(n)),
    construct = construct,
    facet = rep_len(facet, n),
    type = "continuous",
    status = rep_len(status, n),
    stringsAsFactors = FALSE
  )
}

# Every subset of the included items, with the policy applied as stated:
#   - every must-keep item is kept;
#   - no construct loses more than `max_drop` items;
#   - every construct keeps at least min(its minimum, its size) items, so a
#     construct smaller than its minimum keeps all of them;
#   - with a facet minimum, every facet keeps at least min(minimum, its size).
# Returns the membership matrix (one subset per row), which rows are allowed,
# and the item names.
brute_force <- function(meta, max_drop, min_keep, min_facet = NULL,
                        must_keep = character(0)) {
  act <- meta[meta$status == "active", , drop = FALSE]
  n <- nrow(act)
  K <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), n)))
  dimnames(K) <- NULL
  ok <- rep(TRUE, nrow(K))
  protected <- act$item %in% must_keep
  ok <- ok & rowSums(K[, protected, drop = FALSE]) == sum(protected)
  for (cn in unique(act$construct)) {
    in_c <- act$construct == cn
    size <- sum(in_c)
    kept <- rowSums(K[, in_c, drop = FALSE])
    lowest <- if (is.null(names(min_keep))) min_keep else min_keep[[cn]]
    ok <- ok & (size - kept <= max_drop) & (kept >= min(lowest, size))
    if (!is.null(min_facet)) {
      for (f in unique(act$facet[in_c & !is.na(act$facet)])) {
        in_f <- in_c & !is.na(act$facet) & act$facet == f
        ok <- ok & rowSums(K[, in_f, drop = FALSE]) >= min(min_facet, sum(in_f))
      }
    }
  }
  list(K = K, ok = ok, items = act$item)
}

set_key <- function(sets) {
  vapply(sets, function(x) paste(sort(x), collapse = "|"), character(1))
}

brute_sets <- function(bf) {
  lapply(which(bf$ok), function(r) bf$items[bf$K[r, ]])
}

# The small ledgers: two constructs; three constructs with one below the
# usual minimum; facets with an item that has none; and a ledger with
# excluded items, facets, and a construct of one included item.
test_ledgers <- function() {
  list(
    two = ledger(c(rep("A", 4), rep("B", 3))),
    three = ledger(c(rep("A", 4), rep("B", 2), rep("C", 3))),
    facets = ledger(c(rep("A", 6), rep("B", 4)),
      facet = c("f1", "f1", "f2", "f2", "f2", NA, "g1", "g1", "g2", "g2")
    ),
    excluded = ledger(c(rep("A", 5), rep("B", 4), rep("C", 2)),
      facet = c("f1", "f1", "f1", "f2", "f2", NA, NA, NA, NA, "h", "h"),
      status = c(
        "active", "active", "parked", "active", "active",
        rep("active", 4), "active", "parked"
      )
    )
  )
}

must_keep_choices <- function(meta) {
  act <- meta$item[meta$status == "active"]
  list(character(0), act[1], act[c(2, length(act))])
}

# The nine test example with a little missing data, so that the rows each
# statistic uses differ and can be counted: ten cases lack the first Visual
# item, three lack every Speed item, two are empty, and one has only the
# last Speed item.
hs_search_inputs <- function() {
  fx <- sieve_hs9_fixture()
  items <- fx$data[, fx$item_cols, drop = FALSE]
  items[] <- lapply(items, as.numeric)
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  items[1:10, "t1_visual_perception"] <- NA
  items[11:13, meta$item[meta$construct == "Speed"]] <- NA
  items[14:15, ] <- NA
  items[16, setdiff(names(items), "t13_straight_and_curved_capitals")] <- NA
  # One item per construct may be dropped; the other six are protected.
  free <- c("t1_visual_perception", "t9_word_meaning", "t13_straight_and_curved_capitals")
  list(items = items, meta = meta, free = free, must_keep = setdiff(meta$item, free))
}

hs_cache <- new.env()
hs_search <- function() {
  if (is.null(hs_cache$out)) {
    inp <- hs_search_inputs()
    hs_cache$inp <- inp
    hs_cache$out <- sieve_optimize_items(
      inp$items, inp$meta,
      max_drop = 1L, min_keep = 2L, cap = 50L,
      simple_structure = TRUE, must_keep = inp$must_keep, seed = 20261012
    )
  }
  hs_cache
}

# The same protected items on the complete example, without simple structure:
# eight candidates, nothing random, so the module and a direct call agree.
hs_complete_search <- function() {
  if (is.null(hs_cache$complete)) {
    fx <- sieve_hs9_fixture()
    meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    free <- c("t1_visual_perception", "t9_word_meaning", "t13_straight_and_curved_capitals")
    hs_cache$protected <- setdiff(meta$item, free)
    hs_cache$complete <- sieve_optimize_items(
      fx$data[, fx$item_cols, drop = FALSE], meta,
      max_drop = 1L, min_keep = 2L,
      cap = 200L, simple_structure = FALSE, must_keep = hs_cache$protected
    )
  }
  hs_cache
}

optimizer_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

# A state holding arbitrary items and their ledger.
plain_state <- function(items, meta) {
  fixture <- list(data = items, item_cols = names(items), theory_map = NULL,
    name = "constructed", labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    s$meta <- meta
    s$roles <- NULL
  })
}

# The text of a rendered piece of the panel.
panel_text <- function(html) trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", html)))

# Data whose sample covariance matrix is exactly S: standard normal draws,
# centered, rotated to an identity covariance matrix, then to S.
exact_data <- function(S, n = 400, seed = 1) {
  Z <- withr::with_seed(seed, matrix(stats::rnorm(n * ncol(S)), n))
  Z <- scale(Z, center = TRUE, scale = FALSE)
  Z <- Z %*% solve(chol(stats::cov(Z)))
  X <- as.data.frame(Z %*% chol(S))
  names(X) <- colnames(S)
  X
}

# Two constructs of four items. `SA` is the correlation matrix of a1 to a4;
# b1 to b4 correlate `rb` with one another and `cross` with the a items.
heywood_inputs <- function(SA, rb, cross) {
  items <- c(paste0("a", 1:4), paste0("b", 1:4))
  SB <- matrix(rb, 4, 4)
  diag(SB) <- 1
  S <- rbind(cbind(SA, matrix(cross, 4, 4)), cbind(matrix(cross, 4, 4), SB))
  dimnames(S) <- list(items, items)
  stopifnot(all(eigen(S)$values > 0))
  meta <- data.frame(
    item = items, label = items, construct = rep(c("A", "B"), each = 4),
    facet = NA_character_, type = "continuous", status = "active",
    stringsAsFactors = FALSE
  )
  list(items = exact_data(S), meta = meta, S = S)
}

# Once a4 is dropped, A's items correlate .51, .51, and .09: the one-factor
# solution has loadings 1.7, .3, .3, a residual variance of 1 - 1.7^2 for
# a1, and an omega above 1. (The correlations of a4 are chosen so that no
# other three of A's items sit on the boundary of a zero residual variance.)
heywood_above_one <- function() {
  heywood_inputs(matrix(c(
    1, .51, .51, .35, .51, 1, .09, .20, .51, .09, 1, .20,
    .35, .20, .20, 1
  ), 4), rb = .6, cross = .10)
}

# Once a4 is dropped, A's items correlate .65, .65, and .35: the residual
# variance of a1 is negative, omega is inside 0 to 1, and the two-factor
# confirmatory solution of the candidate is proper.
heywood_inside <- function() {
  heywood_inputs(matrix(c(
    1, .65, .65, .5, .65, 1, .35, .4, .65, .35, 1, .4,
    .5, .4, .4, 1
  ), 4), rb = .7, cross = .30)
}

heywood_search <- function(which) {
  key <- paste0("heywood_", which)
  if (is.null(hs_cache[[key]])) {
    inp <- if (identical(which, "above")) heywood_above_one() else heywood_inside()
    hs_cache[[key]] <- list(inp = inp, out = sieve_optimize_items(
      inp$items, inp$meta,
      max_drop = 1L, min_keep = 3L, cap = 300L, simple_structure = FALSE
    ))
  }
  hs_cache[[key]]
}

# The residual variances of the one-factor model of three indicators, in
# closed form: the model reproduces their covariance matrix exactly, so the
# squared loading of item 1 is s12 s13 / s23, and so on.
closed_form_one_factor <- function(X) {
  S <- stats::cov(X)
  lam2 <- c(S[1, 2] * S[1, 3] / S[2, 3], S[1, 2] * S[2, 3] / S[1, 3], S[1, 3] * S[2, 3] / S[1, 2])
  list(residual = diag(S) - lam2, omega = sum(sqrt(lam2))^2 / sum(S))
}

# The column names of the displayed table, so a test names a column instead
# of counting to it.
display_columns <- function() {
  c(
    "position", "order", "id", "n_items", "dropped", "status", "per_construct",
    "omega_min", "omega_mean", "omega_by", "omega_n", "cfi", "rmsea", "rmsea_ci", "srmr",
    "fit_n", "complexity", "hyperplane", "structure_n"
  )
}

# ---------------------------------------------------------------------------
# Candidate generation against brute force
# ---------------------------------------------------------------------------

test_that("the candidates and their count equal a brute-force listing under every constraint", {
  problems <- character(0)
  n_settings <- 0L
  for (nm in names(test_ledgers())) {
    meta <- test_ledgers()[[nm]]
    active <- meta$item[meta$status == "active"]
    for (max_drop in 0:3) {
      for (min_keep in 1:4) {
        for (min_facet in list(NULL, 1L, 2L)) {
          for (must in must_keep_choices(meta)) {
            n_settings <- n_settings + 1L
            tag <- sprintf(
              "%s drop=%d keep=%d facet=%s must=%s", nm, max_drop, min_keep,
              if (is.null(min_facet)) "none" else min_facet,
              paste(must, collapse = "+")
            )
            bf <- brute_force(meta, max_drop, min_keep, min_facet, must)
            con <- sieve_optimizer_constraints(
              max_drop = max_drop, min_keep = min_keep,
              min_facet = min_facet, must_keep = must
            )
            plan <- sieve_optimizer_plan(meta, con)
            got <- sieve_enumerate_candidates(plan)
            counted <- siever_test_optimizer_total(meta, max_drop, min_keep,
              min_facet = min_facet,
              must_keep = must
            )
            dropped <- length(active) - rowSums(bf$K[bf$ok, , drop = FALSE])
            by_drops <- as.numeric(table(factor(dropped, levels = 0:max(dropped))))
            if (!is.null(plan$error)) problems <- c(problems, paste(tag, "refused"))
            if (!setequal(set_key(got), set_key(brute_sets(bf)))) {
              problems <- c(problems, paste(tag, "candidate sets differ"))
            }
            if (anyDuplicated(set_key(got))) problems <- c(problems, paste(tag, "duplicates"))
            if (!identical(counted, as.numeric(sum(bf$ok)))) {
              problems <- c(problems, paste(tag, "count differs"))
            }
            if (!identical(plan$total, as.numeric(length(got)))) {
              problems <- c(problems, paste(tag, "plan total differs from what was built"))
            }
            if (!isTRUE(all.equal(plan$by_drops, by_drops))) {
              problems <- c(problems, paste(tag, "count by number dropped differs"))
            }
            # The current item set is first, exactly, in ledger order.
            if (!identical(got[[1]], active)) problems <- c(problems, paste(tag, "baseline"))
            # Search order: fewest items dropped first.
            if (is.unsorted(length(active) - lengths(got))) {
              problems <- c(problems, paste(tag, "not ordered by items dropped"))
            }
          }
        }
      }
    }
  }
  expect_identical(n_settings, 4L * 4L * 4L * 3L * 3L)
  expect_identical(problems, character(0))
})

test_that("a minimum given per construct is honored construct by construct", {
  meta <- test_ledgers()$three
  min_keep <- c(A = 2L, B = 1L, C = 3L)
  bf <- brute_force(meta, max_drop = 2L, min_keep = min_keep)
  con <- sieve_optimizer_constraints(max_drop = 2L, min_keep = min_keep)
  plan <- sieve_optimizer_plan(meta, con)
  got <- sieve_enumerate_candidates(plan)
  expect_setequal(set_key(got), set_key(brute_sets(bf)))
  expect_identical(plan$total, as.numeric(sum(bf$ok)))
  # A: 1 + 4 + 6 ways to drop up to two of four; B: 1 + 2; C: nothing.
  expect_identical(plan$constructs$n_subsets, c(11, 3, 1))
  expect_identical(plan$total, 33)
})

test_that("with no item allowed to drop, the search is exactly the current item set", {
  for (meta in test_ledgers()) {
    active <- meta$item[meta$status == "active"]
    for (min_keep in c(1L, 3L, 6L)) {
      for (min_facet in list(NULL, 2L)) {
        con <- sieve_optimizer_constraints(
          max_drop = 0L, min_keep = min_keep,
          min_facet = min_facet
        )
        plan <- sieve_optimizer_plan(meta, con)
        expect_null(plan$error)
        expect_identical(plan$total, 1)
        expect_identical(sieve_enumerate_candidates(plan), list(active))
        expect_identical(siever_test_optimizer_total(meta, 0L, min_keep, min_facet = min_facet), 1)
      }
    }
  }
})

test_that("every candidate satisfies every constraint, and the checker agrees with the independent predicate on every subset", {
  settings <- list(
    list(max_drop = 1L, min_keep = 3L, min_facet = NULL, must = 1L),
    list(max_drop = 2L, min_keep = 2L, min_facet = 2L, must = integer(0)),
    list(max_drop = 3L, min_keep = 1L, min_facet = 1L, must = c(2L, 4L)),
    list(max_drop = 0L, min_keep = 4L, min_facet = NULL, must = integer(0))
  )
  disagreements <- character(0)
  n_rejected <- 0L
  for (nm in names(test_ledgers())) {
    meta <- test_ledgers()[[nm]]
    active <- meta$item[meta$status == "active"]
    for (s in settings) {
      must <- active[s$must]
      bf <- brute_force(meta, s$max_drop, s$min_keep, s$min_facet, must)
      con <- sieve_optimizer_constraints(
        max_drop = s$max_drop, min_keep = s$min_keep,
        min_facet = s$min_facet, must_keep = must
      )
      verdict <- vapply(seq_len(nrow(bf$K)), function(r) {
        sieve_check_candidate(bf$items[bf$K[r, ]], meta, con)$ok
      }, logical(1))
      n_rejected <- n_rejected + sum(!verdict)
      if (!identical(verdict, bf$ok)) {
        disagreements <- c(disagreements, nm)
      }
      built <- sieve_enumerate_candidates(sieve_optimizer_plan(meta, con))
      passes <- vapply(built, function(k) sieve_check_candidate(k, meta, con)$ok, logical(1))
      if (!all(passes)) disagreements <- c(disagreements, paste(nm, "built a violator"))
    }
  }
  expect_identical(disagreements, character(0))
  # The comparison is not vacuous: most subsets violate something.
  expect_gt(n_rejected, 1000L)
})

test_that("the checker names what a bad item set violates", {
  meta <- test_ledgers()$facets
  con <- sieve_optimizer_constraints(
    max_drop = 1L, min_keep = 3L, min_facet = 2L,
    must_keep = "i01"
  )
  all_items <- meta$item
  expect_true(sieve_check_candidate(all_items, meta, con)$ok)
  expect_identical(sieve_check_candidate(all_items, meta, con)$violations, character(0))
  # Drops the must-keep item (and with it takes facet f1 below its minimum).
  v <- sieve_check_candidate(setdiff(all_items, "i01"), meta, con)
  expect_false(v$ok)
  expect_match(paste(v$violations, collapse = " "), "must-keep item i01", fixed = TRUE)
  expect_match(paste(v$violations, collapse = " "), "Facet f1 of A", fixed = TRUE)
  # Removes a whole construct.
  v <- sieve_check_candidate(all_items[1:6], meta, con)
  expect_false(v$ok)
  expect_match(paste(v$violations, collapse = " "), "Construct B would keep 0 of its 4 items",
    fixed = TRUE
  )
  # Drops two from one construct when one is allowed.
  v <- sieve_check_candidate(setdiff(all_items, c("i03", "i06")), meta, con)
  expect_match(paste(v$violations, collapse = " "), "at most 1 may be dropped per construct",
    fixed = TRUE
  )
  # An item set from some other pool (a stale candidate) does not pass.
  v <- sieve_check_candidate(c(all_items, "not_in_this_ledger"), meta, con)
  expect_false(v$ok)
  expect_match(paste(v$violations, collapse = " "), "not currently included", fixed = TRUE)
  expect_false(sieve_check_candidate(character(0), meta, con)$ok)
})

# ---------------------------------------------------------------------------
# Held constructs and infeasible constraints
# ---------------------------------------------------------------------------

test_that("a construct below the minimum is held intact and reported, never removed", {
  meta <- ledger(c(rep("A", 4), rep("B", 2)))
  for (max_drop in 0:2) {
    con <- sieve_optimizer_constraints(max_drop = max_drop, min_keep = 3L)
    plan <- sieve_optimizer_plan(meta, con)
    expect_null(plan$error)
    b <- plan$constructs[plan$constructs$construct == "B", ]
    expect_identical(b$state, "held")
    expect_identical(b$detail, "held: too few items to exclude any (2 items, minimum 3)")
    expect_identical(b$max_droppable, 0L)
    built <- sieve_enumerate_candidates(plan)
    expect_true(all(vapply(built, function(k) all(c("i05", "i06") %in% k), logical(1))))
    # A may lose at most one of its four items (it must keep three).
    expect_identical(length(built), if (max_drop == 0L) 1L else 5L)
  }
})

test_that("constraints that cannot be met are refused, naming the constructs and the constraint", {
  meta <- test_ledgers()$excluded
  data <- as.data.frame(matrix(0, 4, nrow(meta), dimnames = list(NULL, meta$item)))

  # The researcher asks that a minimum be a requirement: B and C cannot meet 5.
  r <- sieve_optimize_items(data, ledger(c(rep("A", 4), rep("B", 2))),
    max_drop = 1L,
    min_keep = 3L, undersized = "refuse"
  )
  expect_match(r$error, "The search was not run", fixed = TRUE)
  expect_match(r$error, "construct B has 2 items but its minimum is 3", fixed = TRUE)
  expect_identical(r$evaluated, 0L)
  expect_null(r$table)
  expect_null(r$kept)
  expect_identical(siever_test_optimizer_total(ledger(c(rep("A", 4), rep("B", 2))), 1L, 3L,
    undersized = "refuse"
  ), 0)

  # A facet minimum treated as a requirement names the facet and its construct.
  r <- sieve_optimize_items(data, meta,
    max_drop = 1L, min_keep = 1L, min_facet = 2L,
    undersized = "refuse"
  )
  expect_match(r$error, "facet h of C has 1 item but the facet minimum is 2", fixed = TRUE)

  # A must-keep item that is excluded, and one that does not exist.
  r <- sieve_optimize_items(data, meta,
    max_drop = 1L, min_keep = 2L,
    must_keep = c("i03", "zz")
  )
  expect_match(r$error, "must-keep list cannot be honored", fixed = TRUE)
  expect_match(r$error, "Currently excluded: i03", fixed = TRUE)
  expect_match(r$error, "Not in the item ledger: zz", fixed = TRUE)
  expect_identical(r$evaluated, 0L)

  # A minimum for a construct with no included items.
  r <- sieve_optimize_items(data, meta,
    max_drop = 1L,
    min_keep = c(A = 2L, B = 2L, C = 1L, D = 3L)
  )
  expect_match(r$error, "A minimum was set for D", fixed = TRUE)
  expect_match(r$error, "no included items", fixed = TRUE)

  # Invalid settings are refusals with a reason, not R errors.
  for (bad in list(
    list(max_drop = -1L), list(max_drop = 1.5), list(min_keep = 0L),
    list(min_facet = -2L), list(cap = NA_real_), list(seed = "a")
  )) {
    r <- do.call(sieve_optimize_items, c(list(data, meta), bad))
    expect_match(r$error, "The search was not run", fixed = TRUE)
    expect_identical(r$evaluated, 0L)
  }
  expect_error(sieve_optimizer_constraints(max_drop = NA), "maximum number of items to exclude")

  # A ledger whose included items are not all in the data.
  r <- sieve_optimize_items(data[, -1], meta, max_drop = 0L, min_keep = 1L)
  expect_match(r$error, "do not contain the included item i01", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# The capacity limit protects before anything is built
# ---------------------------------------------------------------------------

test_that("a search above the cap is refused from the algebraic count, without calling the enumerator", {
  big <- ledger(rep(c("A", "B"), each = 16))
  data <- as.data.frame(matrix(0, 5, 32, dimnames = list(NULL, big$item)))
  expected <- sum(choose(16, 8:16))^2

  # Neither the plan nor the count contains a call that builds combinations.
  for (f in list(
    sieve_optimizer_plan, siever_test_optimizer_total, sieve_poly_product,
    sieve_optimizer_constraints, sieve_optimizer_pool, sieve_binomials,
    sieve_plan_alternatives
  )) {
    expect_false(any(c(
      "combn", "sieve_combn", "expand.grid", "sieve_enumerate_candidates",
      "sieve_construct_drop_sets"
    ) %in% all.names(body(f))))
  }

  testthat::local_mocked_bindings(
    sieve_enumerate_candidates = function(...) stop("the enumerator was called"),
    sieve_construct_drop_sets = function(...) stop("the enumerator was called"),
    sieve_combn = function(...) stop("combinations were built"),
    .package = "SIEVER"
  )
  expect_identical(siever_test_optimizer_total(big, max_drop = 8L, min_keep = 3L), expected)
  r <- sieve_optimize_items(data, big, max_drop = 8L, min_keep = 3L, cap = 300L)
  expect_match(r$error, "no candidate was built or fit", fixed = TRUE)
  expect_match(r$error, "1,536,875,209 candidate item sets, above the cap of 300", fixed = TRUE)
  expect_identical(r$total, expected)
  expect_identical(r$evaluated, 0L)
  expect_false(isTRUE(r$complete))
  expect_null(r$table)
  expect_null(r$kept)
  # The refusal says which smaller setting would fit, from counts alone.
  expect_identical(r$alternatives$max_drop, 0:7)
  expect_identical(
    r$alternatives$total,
    vapply(0:7, function(d) sum(choose(16, 0:d))^2, numeric(1))
  )
  expect_match(r$error, "Dropping at most 1 per construct would give 289 candidates",
    fixed = TRUE
  )

  # A pool far too large to enumerate is still counted at once.
  huge <- ledger(rep(sprintf("C%d", 1:6), each = 80))
  expect_equal(siever_test_optimizer_total(huge, max_drop = 40L, min_keep = 3L),
    sum(choose(80, 0:40))^6,
    tolerance = 1e-12
  )

  # Positive control: the mocks are live, so a search within the cap does
  # reach the enumerator. The refusals above therefore never did.
  expect_error(
    sieve_optimize_items(data, big, max_drop = 0L, min_keep = 3L, cap = 300L),
    "the enumerator was called"
  )
})

test_that("a refusal costs one plan whatever maximum to drop is asked for, and its smaller counts equal brute force", {
  # The counts for smaller maximums are read from the one plan. They are
  # compared here with a brute-force listing under each smaller maximum.
  problems <- character(0)
  n_compared <- 0L
  for (nm in names(test_ledgers())) {
    meta <- test_ledgers()[[nm]]
    for (max_drop in 1:4) {
      for (min_keep in 1:3) {
        for (min_facet in list(NULL, 1L, 2L)) {
          for (must in must_keep_choices(meta)) {
            con <- sieve_optimizer_constraints(
              max_drop = max_drop, min_keep = min_keep,
              min_facet = min_facet, must_keep = must
            )
            plan <- sieve_optimizer_plan(meta, con)
            alt <- sieve_plan_alternatives(plan)
            tag <- sprintf(
              "%s drop=%d keep=%d facet=%s must=%s", nm, max_drop, min_keep,
              if (is.null(min_facet)) "none" else min_facet,
              paste(must, collapse = "+")
            )
            # The table covers every maximum below the most a construct can
            # lose, and no more.
            if (!identical(alt$max_drop, seq_len(plan$most_droppable) - 1L)) {
              problems <- c(problems, paste(tag, "rows"))
            }
            if (plan$most_droppable > max_drop) problems <- c(problems, paste(tag, "bound"))
            for (d in alt$max_drop) {
              n_compared <- n_compared + 1L
              brute <- as.numeric(sum(brute_force(meta, d, min_keep, min_facet, must)$ok))
              if (!identical(alt$total[alt$max_drop == d], brute)) {
                problems <- c(problems, paste(tag, "count at", d))
              }
            }
            # At and above the bound the count stays the same.
            at_bound <- as.numeric(sum(brute_force(
              meta, plan$most_droppable, min_keep,
              min_facet, must
            )$ok))
            if (!identical(plan$total, at_bound)) problems <- c(problems, paste(tag, "plateau"))
          }
        }
      }
    }
  }
  expect_identical(problems, character(0))
  expect_gt(n_compared, 500L)

  # One construct of 60 items: whatever maximum is asked for, a refusal
  # plans once, and the table of smaller settings stops at what the
  # construct can lose (57 with a minimum of three). One plan per unit of the
  # maximum would grow without bound with a typed number. The stand-ins below
  # stop the test at once if the search is planned repeatedly (or if
  # anything is built), so such a failure ends here instead of running on.
  big <- ledger(rep("F", 60))
  data <- as.data.frame(matrix(0, 5, 60, dimnames = list(NULL, big$item)))
  plan_once <- sieve_optimizer_plan
  calls <- new.env()
  testthat::local_mocked_bindings(
    sieve_optimizer_plan = function(...) {
      calls$n <- calls$n + 1L
      if (calls$n > 3L) stop("one request planned the search more than three times")
      plan_once(...)
    },
    sieve_enumerate_candidates = function(...) stop("the enumerator was called"),
    sieve_construct_drop_sets = function(...) stop("the enumerator was called"),
    sieve_combn = function(...) stop("combinations were built"),
    .package = "SIEVER"
  )
  for (max_drop in c(30, 1000, 1e6)) {
    calls$n <- 0L
    r <- sieve_optimize_items(data, big, max_drop = max_drop, min_keep = 3L, cap = 300L)
    expect_identical(calls$n, 1L)
    expect_match(r$error, "no candidate was built or fit", fixed = TRUE)
    expect_identical(r$evaluated, 0L)
    expect_null(r$table)
    expect_identical(r$alternatives$max_drop, seq_len(min(max_drop, 57)) - 1L)
    expect_equal(r$alternatives$total,
      vapply(r$alternatives$max_drop, function(d) sum(choose(60, 0:d)), numeric(1)),
      tolerance = 1e-12
    )
    # 1, 61, 1,831: the largest setting within the cap of 300 drops one.
    expect_identical(r$alternatives$total[1:3], c(1, 61, 1831))
    expect_match(r$error, "Dropping at most 1 per construct would give 61 candidates",
      fixed = TRUE
    )
  }
  # A maximum that is not a number of items at all is refused, not coerced.
  calls$n <- 0L
  r <- sieve_optimize_items(data, big, max_drop = 1e12, min_keep = 3L, cap = 300L)
  expect_identical(calls$n, 0L)
  expect_match(r$error, "too large to be a number of items", fixed = TRUE)
  expect_identical(siever_test_optimizer_total(big, 1e12, 3L), 0)
  expect_identical(siever_test_optimizer_total(big, 1e6, 3L), siever_test_optimizer_total(big, 57L, 3L))
})

test_that("a count is called exact only when it is, and is stated approximately otherwise", {
  # The binomial coefficients against Pascal's triangle, which only adds and
  # is therefore exact for whole numbers below 2^53.
  pascal <- 1
  for (m in 1:56) {
    pascal <- c(1, pascal[-1] + pascal[-length(pascal)], 1)
    # The recurrence is exact up to the first entry whose step would pass
    # 2^53 (none before m = 51).
    safe <- which(cumsum(seq_along(pascal) * pascal >= 2^53) == 0)
    expect_identical(sieve_binomials(m, m)[safe], pascal[safe])
    if (m <= 50) expect_length(safe, m + 1L)
  }
  expect_identical(sieve_binomials(7, 0), 1)
  expect_identical(sieve_binomials(0, 0), 1)
  expect_identical(sieve_binomials(5, 2), c(1, 5, 10))

  # These requests are far above the cap and must be refused from the count.
  # If they ever reached the enumerator they would not return, so it is
  # replaced by a function that stops.
  testthat::local_mocked_bindings(
    sieve_enumerate_candidates = function(...) stop("the enumerator was called"),
    sieve_construct_drop_sets = function(...) stop("the enumerator was called"),
    sieve_combn = function(...) stop("combinations were built"),
    .package = "SIEVER"
  )
  refusal <- function(n, max_drop) {
    meta <- ledger(rep("F", n))
    data <- as.data.frame(matrix(0, 3, n, dimnames = list(NULL, meta$item)))
    sieve_optimize_items(data, meta, max_drop = max_drop, min_keep = 3L, cap = 300L)
  }
  # 40 items, up to 37 dropped: 2^40 less the three largest subsets' counts
  # (780 + 40 + 1). Small enough for every step to be exact, and printed to
  # the last digit.
  r <- refusal(40, 37)
  expect_identical(r$total, 2^40 - 821)
  expect_true(r$exact)
  expect_match(r$error, "define 1,099,511,626,955 candidate item sets", fixed = TRUE)
  expect_match(r$error, "The count is exact", fixed = TRUE)

  # 60 items, up to 30 dropped: (2^60 + C(60, 30)) / 2, about 6.36e17, above
  # 2^53. No double holds it exactly, so it is not called exact.
  r <- refusal(60, 30)
  expect_equal(r$total, (2^60 + choose(60, 30)) / 2, tolerance = 1e-12)
  expect_gt(r$total, 2^53)
  expect_false(r$exact)
  expect_match(r$error, "define about 6.36e+17 candidate item sets", fixed = TRUE)
  expect_false(grepl("exact", r$error, fixed = TRUE))
  expect_match(r$error, "stated approximately", fixed = TRUE)

  # 2,000 items: the count overflows a double.
  r <- refusal(2000, 1000)
  expect_identical(r$total, Inf)
  expect_false(r$exact)
  expect_match(r$error, "define more than 1.79e+308 candidate item sets", fixed = TRUE)
  expect_false(grepl("exact", r$error, fixed = TRUE))
  expect_false(grepl("Inf", r$error, fixed = TRUE))

  # The rule itself: exact when the count times the number of items is
  # below 2^53.
  small <- sieve_optimizer_plan(
    ledger(rep(c("A", "B"), each = 16)),
    sieve_optimizer_constraints(max_drop = 8L, min_keep = 3L)
  )
  expect_true(small$exact)
  expect_identical(sieve_count_text(small$total, small$exact), "1,536,875,209")
  expect_identical(sieve_count_text(268800), "268,800")
  expect_identical(sieve_count_text(6.35593e17, FALSE), "about 6.36e+17")
  expect_identical(sieve_count_text(Inf, FALSE), "more than 1.79e+308")
})

test_that("a search within the cap examines every candidate and says so", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  h <- hs_search()
  out <- h$out
  expect_null(out$error)
  expect_true(out$complete)
  expect_identical(out$total, 8)
  expect_identical(out$evaluated, 8L)
  expect_identical(nrow(out$table), 8L)
  expect_length(out$kept, 8L)
  # The fields the cached result and the apply step read are all present.
  expect_true(all(c("table", "kept", "total", "evaluated", "notes") %in% names(out)))
  expect_identical(out$table$id, 1:8)
  # Progress is reported once per candidate.
  seen <- new.env()
  seen$i <- integer(0)
  inp <- h$inp
  two <- sieve_optimize_items(inp$items, inp$meta,
    max_drop = 1L, min_keep = 2L,
    simple_structure = FALSE,
    must_keep = setdiff(inp$meta$item, inp$free[1]),
    progress = function(i) seen$i <- c(seen$i, i)
  )
  expect_identical(seen$i, 1:2)
})

# ---------------------------------------------------------------------------
# A pool whose constraints allow no drops keeps every item
# ---------------------------------------------------------------------------

test_that("four items in A and two in B with no drops allowed keeps the whole pool", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  # The first six tests, A = four items, B = two, max_drop = 0,
  # min_keep = 3: the result is the whole pool, not a "baseline" of A alone
  # with B's two items listed as dropped.
  fx <- sieve_hs9_fixture()
  six <- fx$item_cols[1:6]
  meta <- sieve_build_meta(fx$data, six, NULL)
  meta$construct <- c(rep("A", 4), rep("B", 2))
  items <- fx$data[, six, drop = FALSE]

  expect_identical(siever_test_optimizer_total(meta, max_drop = 0L, min_keep = 3L), 1)
  out <- sieve_optimize_items(items, meta,
    max_drop = 0L, min_keep = 3L,
    simple_structure = FALSE
  )
  expect_null(out$error)
  expect_identical(nrow(out$table), 1L)
  expect_identical(out$kept, list(six))
  expect_identical(out$table$role, "baseline")
  expect_identical(out$table$n_items, 6L)
  expect_identical(out$table$n_dropped, 0L)
  expect_identical(out$table$dropped, "(none)")
  expect_identical(out$table$per_construct, "A:4 B:2")
  held <- out$constructs[out$constructs$state == "held", ]
  expect_identical(held$construct, "B")
  expect_match(paste(out$notes, collapse = " "),
    "B: held: too few items to exclude any (2 items, minimum 3)",
    fixed = TRUE
  )
  # B's reliability is not estimable from two items, and the table says so
  # instead of reporting A's omega as if it were the instrument's.
  expect_true(is.na(out$table$omega_min))
  expect_false(out$table$reliability_ok)
  expect_match(out$table$reason, "omega unavailable (B would have 2 items", fixed = TRUE)
  expect_true(is.finite(out$omega[1, "A"]))

  # Allowing one drop prunes A only; B is in every candidate.
  one <- sieve_optimize_items(items, meta,
    max_drop = 1L, min_keep = 3L,
    simple_structure = FALSE
  )
  expect_identical(nrow(one$table), 5L)
  expect_identical(one$kept[[1]], six)
  expect_true(all(vapply(one$kept, function(k) all(six[5:6] %in% k), logical(1))))
  expect_true(all(grepl("B:2", one$table$per_construct, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# What the statistics are, and the rows they were computed on
# ---------------------------------------------------------------------------

test_that("the statistics of the current item set match independent computations on the stated rows", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  skip_if_not_installed("psych")
  h <- hs_search()
  out <- h$out
  items <- h$inp$items
  meta <- h$inp$meta
  expect_identical(out$kept[[1]], meta$item)

  # Omega: with three indicators the one-factor model reproduces the
  # covariance matrix exactly, so the loadings have a closed form and
  # omega = (sum of loadings)^2 / (sum of all variances and covariances).
  closed_omega <- function(X) {
    S <- stats::cov(X)
    lam <- c(
      sqrt(S[1, 2] * S[1, 3] / S[2, 3]),
      sqrt(S[1, 2] * S[2, 3] / S[1, 3]),
      sqrt(S[1, 3] * S[2, 3] / S[1, 2])
    )
    sum(lam)^2 / sum(S)
  }
  cons <- unique(meta$construct)
  want <- vapply(cons, function(cn) {
    X <- items[, meta$item[meta$construct == cn], drop = FALSE]
    closed_omega(X[stats::complete.cases(X), , drop = FALSE])
  }, numeric(1))
  n_want <- vapply(cons, function(cn) {
    sum(stats::complete.cases(items[, meta$item[meta$construct == cn], drop = FALSE]))
  }, integer(1))
  expect_equal(out$omega[1, cons], want, tolerance = 1e-6)
  expect_identical(out$omega_n[1, cons], n_want)
  expect_identical(unname(n_want), c(288L, 298L, 295L))
  expect_equal(out$table$omega_min[1], min(want), tolerance = 1e-6)
  expect_equal(out$table$omega_mean[1], mean(want), tolerance = 1e-6)
  expect_identical(out$table$omega_n_min[1], 288L)
  expect_identical(out$table$omega_n_max[1], 298L)

  # Fit: the correlated factors model, ML with full information ML (what
  # Follow the item types gives continuous items); rows with at least one
  # observed kept item.
  model <- paste(vapply(cons, function(cn) {
    sprintf("%s =~ %s", cn, paste(meta$item[meta$construct == cn], collapse = " + "))
  }, character(1)), collapse = "\n")
  signaled <- new.env()
  signaled$warnings <- character(0)
  direct <- withCallingHandlers(
    lavaan::cfa(model, data = items, estimator = "ML", missing = "fiml", std.lv = TRUE), # Auto for continuous items
    warning = function(w) {
      signaled$warnings <- c(signaled$warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # lavaan says that the two empty cases are ignored, and nothing else.
  expect_length(signaled$warnings, 1L)
  expect_match(signaled$warnings, "empty")
  # CFI and RMSEA are reported in ONE named family of fit indices for every
  # item set (standard, scaled or robust), and the result says which. For
  # ML with FIML on continuous items every item set has the robust indices
  # (lavaan's incomplete data corrections), so that is the family; the
  # values are lavaan's own columns of that family, not the scaled ones
  # under another name.
  expect_identical(out$fit_family, "robust")
  expect_match(out$fit_family_reason, "robust")
  fam <- out$fit_family
  col <- function(stem) sprintf("%s.%s", stem, fam)
  # The RMSEA interval is at 90%, the RMSEA level (DMAR's default for it
  # and lavaan's).
  fm <- lavaan::fitMeasures(direct, c(
    col("cfi"), col("rmsea"), col("rmsea.ci.lower"),
    col("rmsea.ci.upper"), "cfi.scaled", "srmr_bentler_nomean"
  ), fm.args = list(rmsea.ci.level = 0.90))
  expect_equal(out$table$cfi[1], unname(fm[col("cfi")]), tolerance = 1e-8)
  expect_equal(out$table$rmsea[1], unname(fm[col("rmsea")]), tolerance = 1e-8)
  expect_equal(out$table$rmsea_lo[1], unname(fm[col("rmsea.ci.lower")]), tolerance = 1e-8)
  expect_equal(out$table$rmsea_hi[1], unname(fm[col("rmsea.ci.upper")]), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(out$table$cfi[1], unname(fm["cfi.scaled"]), tolerance = 1e-8)))
  # The SRMR of the covariance residuals, DMAR's (lavaan's srmr_bentler_nomean).
  expect_equal(out$table$srmr[1], unname(fm["srmr_bentler_nomean"]), tolerance = 1e-8)
  any_observed <- function(cols) sum(rowSums(!is.na(items[, cols, drop = FALSE])) > 0)
  expect_identical(out$table$fit_n[1], any_observed(meta$item))
  expect_identical(out$table$fit_n[1], 299L)
  expect_identical(out$table$fit_estimator[1], "ML")
  expect_identical(out$table$fit_missing[1], "fiml")
  # What lavaan signaled is kept with the candidate, not discarded.
  expect_match(paste(out$warnings[[1]], collapse = " "), "empty")

  # The rows differ by candidate, as the labels say: dropping the last Speed
  # item removes the case that had nothing else, and dropping the first
  # Visual item restores the ten cases that lacked only it.
  for (i in seq_len(nrow(out$table))) {
    kept <- out$kept[[i]]
    expect_identical(out$table$fit_n[i], any_observed(kept))
    expect_identical(
      out$table$structure_n[i],
      sum(stats::complete.cases(items[, kept, drop = FALSE]))
    )
  }
  expect_identical(out$table$structure_n[1], 285L)
  expect_true(length(unique(out$table$fit_n)) > 1L)
  expect_true(length(unique(out$table$structure_n)) > 1L)

  # Simple structure: exploratory ML, oblimin, Pearson, complete cases, on
  # the recorded seed. The engine is DMAR::fa_exploratory() on the complete
  # rows; psych::fa agrees with it within the optimizers' tolerance.
  X <- items[stats::complete.cases(items), out$kept[[1]], drop = FALSE]
  efa <- sieve_efa_dmar_solution(DMAR::fa_exploratory(data = X, nfactors = 3L, estimator = "ML",
    missing = "listwise", rotation = "oblimin", conf_level = 0.95, seed = out$rng$seed))
  expect_identical(out$rng$seed, 20261012L)
  expect_equal(out$table$complexity[1], mean(efa$complexity), tolerance = 1e-10)
  expect_equal(out$table$hyperplane[1], mean(abs(efa$loadings) < 0.10),
    tolerance = 1e-12
  )
  ps <- withr::with_seed(out$rng$seed, psych::fa(X, nfactors = 3L, fm = "ml", rotate = "oblimin"))
  expect_lt(abs(out$table$complexity[1] - mean(ps$complexity)), 1e-4)
})

test_that("the search can be replayed and leaves the caller's random numbers alone", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  skip_if_not_installed("psych")
  fx <- sieve_hs9_fixture()
  items <- fx$data[, fx$item_cols, drop = FALSE]
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  run <- function(seed = NULL) {
    sieve_optimize_items(items, meta, max_drop = 0L, min_keep = 3L, seed = seed)
  }
  withr::with_seed(11, {
    before <- get(".Random.seed", envir = globalenv())
    a <- run(seed = 5)
    # A supplied seed: the caller's stream is exactly where it was.
    expect_identical(get(".Random.seed", envir = globalenv()), before)
    b <- run(seed = 5)
    expect_identical(a$table, b$table)
    # No seed: one is drawn, recorded, and replays the run exactly.
    c0 <- run()
    expect_false(c0$rng$supplied)
    expect_identical(run(seed = c0$rng$seed)$table, c0$table)
  })
  # Nothing random is used, and nothing is drawn, without simple structure.
  expect_null(sieve_optimize_items(items, meta,
    max_drop = 0L, min_keep = 3L,
    simple_structure = FALSE
  )$rng)
})

test_that("omega here is the continuous, complete-case coefficient whatever the item types, and is labeled so", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  fx <- sieve_hs9_fixture()
  # Four ordered categories per item.
  ord <- as.data.frame(lapply(fx$data[, fx$item_cols], function(v) {
    as.integer(cut(v, stats::quantile(v, 0:4 / 4, na.rm = TRUE), include.lowest = TRUE))
  }))
  # Five cases lack the first item, so the rows each fit uses can be told apart.
  ord[1:5, 1] <- NA
  meta <- sieve_build_meta(ord, names(ord), fx$theory_map)
  as_ordered <- meta
  as_ordered$type <- "ordered"
  as_continuous <- meta
  as_continuous$type <- "continuous"
  o <- sieve_optimize_items(ord, as_ordered,
    max_drop = 0L, min_keep = 3L,
    simple_structure = FALSE
  )
  k <- sieve_optimize_items(ord, as_continuous,
    max_drop = 0L, min_keep = 3L,
    simple_structure = FALSE
  )
  # The estimator of the confirmatory fit follows the item types ...
  expect_identical(o$table$fit_estimator, "WLSMV")
  expect_identical(o$table$fit_missing, "listwise")
  expect_identical(k$table$fit_estimator, "ML")
  expect_identical(k$table$fit_missing, "fiml")
  expect_false(isTRUE(all.equal(o$table$cfi, k$table$cfi)))
  # ... and so do its rows, as the sample label says: complete rows under
  # WLSMV, every row with an observed item under full information ML.
  expect_identical(o$table$fit_n, sum(stats::complete.cases(ord)))
  expect_identical(k$table$fit_n, sum(rowSums(!is.na(ord)) > 0))
  expect_identical(c(o$table$fit_n, k$table$fit_n), c(296L, 301L))
  # ... and omega does not: it is the same continuous coefficient, on the
  # cases complete on each construct's items, either way.
  expect_identical(o$omega, k$omega)
  expect_identical(o$omega_n, k$omega_n)
  expect_identical(unname(o$omega_n[1, ]), c(296L, 301L, 301L))

  m <- sieve_optimizer_metrics()
  omega_rows <- m[m$family == "reliability", ]
  expect_true(all(grepl("treated as continuous", omega_rows$target, fixed = TRUE)))
  expect_true(all(grepl("complete", omega_rows$sample, fixed = TRUE)))
  expect_true(all(grepl("Pearson", m$target[m$family == "structure"], fixed = TRUE)))
  expect_true(all(grepl("complete", m$sample[m$family == "structure"], fixed = TRUE)))
  expect_true(all(nzchar(m$target)) && all(nzchar(m$sample)))
  expect_identical(sieve_optimizer_requirements()$omega, 3L)
})

# ---------------------------------------------------------------------------
# Eligibility and ordering (rankings)
# ---------------------------------------------------------------------------

test_that("likelihood criteria are not returned or offered for ordering", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  out <- hs_search()$out
  expect_false(any(grepl("aic|bic|loglik", names(out$table), ignore.case = TRUE)))
  expect_false(any(grepl("aic|bic|loglik", sieve_optimizer_metrics()$metric,
    ignore.case = TRUE
  )))
  expect_setequal(
    sieve_optimizer_metrics()$metric,
    c(
      "omega_min", "omega_mean", "cfi", "rmsea", "srmr", "complexity",
      "hyperplane"
    )
  )
})

test_that("every candidate carries an eligibility status and a reason for whatever is unavailable", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  h <- hs_search()
  out <- h$out
  tab <- out$table
  meta <- h$inp$meta
  expect_true(all(tab$status %in% c("eligible", "partly eligible", "not eligible")))
  expect_true(all(nzchar(tab$reason[tab$status != "eligible"])))
  expect_true(all(!nzchar(tab$reason[tab$status == "eligible"])))
  for (i in seq_len(nrow(tab))) {
    kept <- out$kept[[i]]
    n_by <- table(factor(meta$construct[meta$item %in% kept], levels = unique(meta$construct)))
    short <- names(n_by)[n_by < 3L]
    # Omega exists for a construct exactly when it keeps three or more items.
    expect_identical(names(which(is.na(out$omega[i, ]))), short)
    if (length(short)) {
      expect_false(tab$reliability_ok[i])
      expect_true(is.na(tab$omega_min[i]) && is.na(tab$omega_mean[i]))
      expect_match(tab$reliability_note[i], "omega unavailable", fixed = TRUE)
      for (cn in short) {
        expect_match(tab$reliability_note[i], sprintf("%s would have 2 items", cn), fixed = TRUE)
      }
      expect_false(identical(tab$status[i], "eligible"))
    } else {
      expect_true(tab$reliability_ok[i])
      expect_identical(tab$omega_min[i], min(out$omega[i, ]))
    }
    # A fit is orderable only when the solution is a usable one.
    if (!identical(tab$fit_code[i], "ok")) {
      expect_false(tab$fit_ok[i])
      expect_match(tab$fit_note[i], "^fit (not ranked|unavailable)")
    }
    # A status of eligible means every requested family is available.
    expect_identical(
      tab$status[i] == "eligible",
      tab$reliability_ok[i] && tab$fit_ok[i] && tab$structure_ok[i]
    )
  }
  # Only the current item set keeps three items in every construct here.
  expect_identical(which(tab$reliability_ok), 1L)
})

test_that("a candidate fit with a different estimator than the current item set is not ranked on fit", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  fx <- sieve_hs9_fixture()
  items <- fx$data[, fx$item_cols, drop = FALSE]
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  last <- "t13_straight_and_curved_capitals"
  items[[last]] <- as.integer(cut(items[[last]],
    stats::quantile(items[[last]], 0:4 / 4, na.rm = TRUE),
    include.lowest = TRUE
  ))
  meta$type[meta$item == last] <- "ordered"
  out <- sieve_optimize_items(items, meta,
    max_drop = 1L, min_keep = 2L,
    simple_structure = FALSE,
    must_keep = setdiff(meta$item, last)
  )
  expect_identical(out$table$fit_estimator, c("WLSMV", "ML"))
  expect_false(out$table$fit_ok[2])
  expect_match(out$table$fit_note[2],
    "this item set is fit with ML, the current item set with WLSMV",
    fixed = TRUE
  )
  ord <- sieve_order_candidates(out$table, "cfi")
  expect_identical(ord$group, c("reference", "not ranked"))
  expect_match(as.character(sieve_candidate_header(out)),
    "WLSMV, because ordered or binary items are included; complete rows",
    fixed = TRUE
  )
})

test_that("ordering never hides a candidate and never ranks one that lacks the statistic", {
  tab <- data.frame(
    id = 1:7,
    role = c("baseline", rep("candidate", 6)),
    omega_min = c(0.70, 0.80, NA, 0.75, 0.90, 0.85, 0.85),
    omega_mean = c(0.72, 0.81, NA, 0.76, 0.91, 0.86, 0.86),
    reliability_ok = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE),
    cfi = c(0.93, 0.95, 0.99, NA, 0.94, 0.95, 0.90),
    rmsea = c(0.08, 0.05, 0.02, NA, 0.06, 0.05, 0.07),
    srmr = c(0.06, 0.05, 0.02, NA, 0.05, 0.05, 0.06),
    fit_ok = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
    complexity = NA_real_,
    hyperplane = NA_real_,
    structure_ok = FALSE,
    stringsAsFactors = FALSE
  )
  shown <- function(by) {
    o <- sieve_order_candidates(tab, by)
    expect_setequal(o$position, 1:7) # every candidate is listed once
    expect_identical(o$id, tab$id) # aligned to the table's rows
    o[order(o$position), ]
  }

  # The default favors no statistic: search order.
  s <- shown("search")
  expect_identical(s$id, 1:7)
  expect_identical(s$label, c("reference", rep("search order", 6)))
  expect_identical(sieve_order_candidates(tab)$position, 1:7)
  expect_identical(shown("not_a_metric")$id, 1:7)
  expect_identical(shown("aic")$id, 1:7)

  # min omega, highest first. Candidate 5 has the largest value but its
  # reliability is not available as a comparable summary; candidate 3 has
  # none. Neither is ranked; both are listed after the ranked ones.
  o <- shown("omega_min")
  expect_identical(o$id, c(1L, 6L, 7L, 2L, 4L, 3L, 5L))
  expect_identical(o$group, c("reference", rep("ranked", 4), rep("not ranked", 2)))
  expect_identical(o$label, c("reference", "1", "1", "3", "4", "not ranked", "not ranked"))

  # RMSEA, lowest first. Candidate 3 has the smallest value and an unusable
  # solution; a blank or an unusable value must not sort to the top.
  o <- shown("rmsea")
  expect_identical(o$id, c(1L, 2L, 6L, 5L, 7L, 3L, 4L))
  expect_identical(o$label, c("reference", "1", "1", "3", "4", "not ranked", "not ranked"))
  expect_identical(shown("cfi")$id, c(1L, 2L, 6L, 5L, 7L, 3L, 4L))

  # A statistic nobody has: the reference, then everyone, not ranked.
  o <- shown("complexity")
  expect_identical(o$id, 1:7)
  expect_identical(o$group, c("reference", rep("not ranked", 6)))

  # The reference stays first even when it lacks the statistic itself.
  tab$reliability_ok[1] <- FALSE
  tab$omega_min[1] <- NA
  expect_identical(shown("omega_min")$id[1], 1L)
  expect_identical(shown("omega_min")$group[1], "reference")
})

test_that("the displayed table has no blank cell and marks what is not ranked", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  out <- hs_search()$out
  for (by in c("search", sieve_optimizer_metrics()$metric)) {
    d <- sieve_candidate_display(out, by)
    expect_identical(nrow(d), nrow(out$table))
    expect_false(anyNA(d))
    expect_true(all(nzchar(trimws(as.matrix(d[, -1])))))
    expect_identical(d$position[1], 1L)
    # The reference is first. When the table is ordered by a statistic, the
    # order cell also carries that statistic's value as shown in its column.
    expect_identical(d$order[1], if (by == "search" || !is.finite(out$table[[by]][1])) {
      "reference"
    } else {
      sprintf("reference (%s)", d[[by]][1])
    })
  }
  d <- sieve_candidate_display(out, "omega_min")
  no_omega <- !out$table$reliability_ok
  expect_true(all(d$omega_min[no_omega] == "unavailable"))
  expect_true(all(d$order[no_omega] == "not ranked"))
  expect_true(all(grepl("omega unavailable", d$status[no_omega], fixed = TRUE)))
  # The omega of each construct that can be estimated is still shown.
  expect_match(d$omega_by[2], "unavailable", fixed = TRUE)
  expect_match(d$omega_by[2], "[0-9]\\.[0-9]{3}")
  # Rounding happens here and only here: the stored values keep full precision.
  expect_match(d$cfi[1], "^[01]\\.[0-9]{3}$")
  expect_false(isTRUE(all.equal(out$table$cfi[1], round(out$table$cfi[1], 3))))
  # A computed value that is not ranked is shown with a mark, never unmarked.
  marked <- function(tab_ok, shown, value) all(grepl(" \\*$", shown[!tab_ok & is.finite(value)]))
  expect_true(marked(out$table$fit_ok, d$cfi, out$table$cfi))
  expect_true(marked(out$table$fit_ok, d$rmsea, out$table$rmsea))
})

# ---------------------------------------------------------------------------
# Omega from an improper one-factor solution (rankings must not hide it)
# ---------------------------------------------------------------------------

test_that("an omega from an improper one-factor solution is flagged with its reason and never ranked", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  a_items <- paste0("a", 1:4)
  b_items <- paste0("b", 1:4)
  for (which in c("above", "inside")) {
    h <- heywood_search(which)
    out <- h$out
    X <- h$inp$items
    tab <- out$table
    expect_null(out$error)
    expect_identical(nrow(tab), 25L)

    # Which one-factor solutions are improper, decided without the package:
    # for three indicators from the closed form residual variances, for four
    # from a direct lavaan fit. Neither reads what DMAR signals.
    residual_variances <- function(items) {
      if (length(items) == 3L) {
        return(closed_form_one_factor(X[, items, drop = FALSE])$residual)
      }
      # lavaan warns about the negative variance this fit is run to find.
      fit <- withCallingHandlers(
        lavaan::cfa(sprintf("F =~ %s", paste(items, collapse = " + ")), data = X, std.lv = TRUE),
        warning = function(w) invokeRestart("muffleWarning")
      )
      pe <- lavaan::parameterEstimates(fit)
      pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs != "F"]
    }
    residuals <- lapply(out$kept, function(k) {
      list(A = residual_variances(k[k %in% a_items]),
        B = residual_variances(k[k %in% b_items]))
    })
    want <- t(vapply(residuals, function(r) vapply(r, function(x) any(x < 0), logical(1)), logical(2)))
    refused <- t(vapply(residuals, function(r) vapply(r, function(x) sum(x) < 0, logical(1)), logical(2)))
    expected_state <- matrix(ifelse(refused, "no_estimate", ifelse(want, "improper", "ok")),
      nrow = nrow(want), dimnames = dimnames(want))
    expect_identical(unname(out$omega_state), unname(expected_state))
    improper <- rowSums(want) > 0
    unavailable <- rowSums(refused) > 0
    returned_improper <- improper & !unavailable
    expect_identical(sum(improper), if (which == "above") 10L else 5L)
    expect_identical(sum(unavailable), if (which == "above") 5L else 0L)
    expect_false(any(want[, "B"]))

    # An item set with an improper omega is not available on reliability,
    # says why and for which construct, is never called eligible, and is
    # never ranked on min omega or mean omega. Every other item set is.
    expect_identical(tab$reliability_ok, !improper)
    expect_identical(tab$reliability_code, ifelse(unavailable, "no_estimate", ifelse(improper, "improper", "ok")))
    expect_true(all(grepl("omega not ranked (the one-factor model for A gave an improper solution",
      tab$reliability_note[returned_improper],
      fixed = TRUE
    )))
    expect_true(all(!nzchar(tab$reliability_note[!improper])))
    expect_false(any(tab$status[improper] == "eligible"))
    expect_true(all(grepl("omega not ranked", tab$reason[returned_improper], fixed = TRUE)))
    expect_true(all(grepl("improper beyond use", tab$reliability_note[unavailable], fixed = TRUE)))
    for (by in c("omega_min", "omega_mean")) {
      ord <- sieve_order_candidates(tab, by)
      expect_true(all(ord$group[improper & tab$role != "baseline"] == "not ranked"))
      expect_true(all(ord$group[!improper & tab$role != "baseline"] == "ranked"))
      shown <- ord[order(ord$position), ]
      expect_true(max(which(shown$group == "ranked")) < min(which(shown$group == "not ranked")))
    }

    # The value is kept as estimated (not hidden, not clipped), and it is
    # marked wherever it is shown: in the summaries and beside its construct.
    expect_identical(is.na(out$omega), refused)
    expect_true(all(is.finite(out$omega[!refused])))
    expect_identical(tab$omega_min, apply(out$omega, 1, min))
    expect_identical(
      tab$omega_mean,
      vapply(seq_len(nrow(tab)), function(k) mean(out$omega[k, ]), numeric(1))
    )
    d <- sieve_candidate_display(out, "omega_min")
    expect_true(all(grepl(" \\*$", d$omega_min[returned_improper])))
    expect_true(all(grepl(" \\*$", d$omega_mean[returned_improper])))
    expect_true(all(grepl("^A [-0-9.]+ \\*; B [0-9.]+$", d$omega_by[returned_improper])))
    expect_true(all(grepl("^A [0-9.]+; B [0-9.]+$", d$omega_by[!improper])))
    expect_false(any(grepl("*", d$omega_min[!improper], fixed = TRUE)))
    expect_true(all(grepl("^not ranked \\(", d$order[returned_improper & tab$role != "baseline"])))

    expect_true(all(d$order[unavailable & tab$role != "baseline"] == "not ranked"))

    # What DMAR and lavaan said about the one-factor model stays with the
    # candidate, labeled with the construct.
    for (i in which(returned_improper)) {
      # Robust omega (observed denominator): DMAR's observed
      # path passes on lavaan's negative-variance warning rather than its
      # own "Improper solution" message.
      expect_match(paste(out$warnings[[i]], collapse = " | "),
        "omega of A: .*(Improper solution|variances are negative)"
      )
    }
    expect_false(any(grepl("omega of", unlist(out$warnings[!improper]), fixed = TRUE)))
    expect_match(paste(out$notes, collapse = " "),
      sprintf(
        "The omega of %d of 25 candidates comes from an improper one-factor",
        sum(returned_improper)
      ),
      fixed = TRUE
    )

    # The candidate that drops a4: its three A items give the closed form.
    i <- which(tab$dropped == "a4")
    closed <- closed_form_one_factor(X[, c("a1", "a2", "a3")])
    expect_lt(closed$residual[1], 0)
    if (which == "above") {
      # Loadings 1.7, .3, .3 on unit variances: omega = 5.29 / 5.22.
      expect_equal(closed$omega, 5.29 / 5.22, tolerance = 1e-12)
      expect_true(is.na(out$omega[i, "A"]))
      expect_identical(unname(out$omega_state[i, "A"]), "no_estimate")
      expect_match(tab$reliability_note[i], "improper beyond use", fixed = TRUE)
      expect_identical(sieve_order_candidates(tab, "omega_mean")$group[i], "not ranked")
    } else {
      # Inside 0 to 1, with a proper confirmatory solution: this row has the
      # largest min omega but is "partly eligible" and not ranked.
      expect_equal(unname(out$omega[i, "A"]), closed$omega, tolerance = 1e-6)
      expect_gt(out$omega[i, "A"], 0)
      expect_lt(out$omega[i, "A"], 1)
      expect_identical(tab$fit_code[i], "ok")
      expect_true(tab$fit_ok[i])
      expect_identical(tab$status[i], "partly eligible")
      expect_identical(which.max(tab$omega_min), i)
      expect_identical(sieve_order_candidates(tab, "omega_min")$group[i], "not ranked")
    }
  }
})

test_that("the omega estimate keeps what the engines signaled and classifies the solution", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  X <- heywood_search("inside")$inp$items

  # A proper solution: DMAR's robust omega (observed denominator; the
  # Optimizer's coefficient), and nothing signaled.
  robust <- function(Z) {
    # The Heywood case below makes lavaan warn; sieve_omega_estimate() keeps
    # that warning, which the expectations below read.
    r <- suppressWarnings(DMAR::reliability_omega(data = Z, ci_method = "none", denominator = "observed"))
    r$value[r$term == "estimate"][1]
  }
  ok <- sieve_omega_estimate(X[, paste0("b", 1:4)])
  expect_identical(ok$state, "ok")
  expect_null(ok$problem)
  expect_identical(ok$conditions, character(0))
  expect_identical(ok$value, robust(X[, paste0("b", 1:4)]))

  # A Heywood case whose omega is inside 0 to 1 can only be recognized from
  # what DMAR and lavaan signal. This is the check that would fail if their
  # wording changed, instead of the flag being lost silently.
  bad <- sieve_omega_estimate(X[, c("a1", "a2", "a3")])
  expect_lt(closed_form_one_factor(X[, c("a1", "a2", "a3")])$residual[1], 0)
  expect_gt(bad$value, 0)
  expect_lt(bad$value, 1)
  expect_identical(bad$state, "improper")
  expect_identical(bad$problem, "an improper solution, with a negative residual variance estimate")
  expect_true(any(grepl("Improper solution|variances are negative", bad$conditions)))
  expect_equal(bad$value, robust(X[, c("a1", "a2", "a3")]), tolerance = 1e-12)

  # A negative composite error variance is refused even with warnings silenced.
  above <- heywood_search("above")$inp$items
  capture <- sieve_capture
  silent <- testthat::with_mocked_bindings(
    sieve_omega_estimate(above[, c("a1", "a2", "a3")]),
    sieve_capture = function(expr) {
      got <- capture(expr)
      got$warnings <- character(0)
      got
    },
    .package = "SIEVER"
  )
  expect_identical(silent$conditions, character(0))
  expect_true(is.na(silent$value))
  expect_identical(silent$state, "no_estimate")
  expect_match(silent$problem, "improper beyond use", fixed = TRUE)

  # A model that gives no estimate (two identical items) is reported with
  # DMAR's reason, and the candidate's omega is unavailable, not blank.
  dup <- X[, c("b1", "b2", "b3")]
  dup$b3 <- dup$b1
  none <- sieve_omega_estimate(dup)
  expect_identical(none$state, "no_estimate")
  expect_true(is.na(none$value))
  expect_match(none$problem, "sample covariance matrix is not positive-definite", fixed = TRUE)
})

test_that("the module says when omega comes from an improper solution", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  h <- heywood_search("inside")
  st <- plain_state(h$inp$items, h$inp$meta)
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(max_drop = 1, min_keep = 3, ss = FALSE, cap = 300, undersized = "hold")
    session$setInputs(run = 1)
    msg <- panel_text(output$run_msg$html)
    expect_match(msg, "The omega of 5 of 25 item sets comes from an improper one-factor solution",
      fixed = TRUE
    )
    expect_match(msg, "not a usable reliability estimate", fixed = TRUE)
    expect_false(grepl("Omega (min and mean) is unavailable", msg, fixed = TRUE))
    # Ordered by min omega, the improper item sets follow every ranked one.
    session$setInputs(order_by = "omega_min")
    d <- sieve_candidate_display(res(), "omega_min")
    expect_match(output$results, sprintf("\"data\":[[%s],", paste(d$position, collapse = ",")),
      fixed = TRUE
    )
    improper <- res()$table$reliability_code == "improper"
    expect_true(all(d$position[improper] > max(d$position[!improper])))
    # The detail pane gives the reason and what was signaled.
    session$setInputs(results_rows_selected = which(res()$table$dropped == "a4"))
    detail <- panel_text(output$detail$html)
    expect_match(detail, "omega not ranked (the one-factor model for A gave an improper solution",
      fixed = TRUE
    )
    expect_match(detail, "omega of A: .*(Improper solution|variances are negative)")
  })
})

# ---------------------------------------------------------------------------
# The panel
# ---------------------------------------------------------------------------

test_that("the panel states what the search is, labels its columns, and offers no default ranking", {
  html <- as.character(mod_optimize_ui("opt"))
  for (phrase in c(
    "It proposes; you decide",
    "descriptive and selection dependent",
    "A chosen item set needs independent data",
    "treat the item codes as continuous and use complete cases",
    "These are not always the Reliability and EFA tab values",
    "AIC and BIC are not shown",
    "never applies a candidate on its own",
    "one-factor model, which needs at least 3 items per construct",
    "There is no partial search"
  )) {
    expect_match(html, phrase, fixed = TRUE)
  }
  expect_false(grepl("same quantities", html, fixed = TRUE))
  expect_false(grepl("\u2014", html, fixed = TRUE))
  # The researcher's controls: constraints, and an explicit ordering choice
  # whose default is search order, not a fit index.
  for (id in c(
    "opt-max_drop", "opt-min_keep", "opt-min_facet", "opt-must_keep",
    "opt-undersized", "opt-order_by", "opt-cap", "opt-run", "opt-apply"
  )) {
    expect_match(html, sprintf("id=\"%s\"", id), fixed = TRUE)
  }
  expect_match(html, "<option value=\"search\" selected>", fixed = TRUE)
  expect_false(grepl("<option value=\"cfi\" selected>", html, fixed = TRUE))
  # Every statistic in the catalog is described with its target and sample.
  m <- sieve_optimizer_metrics()
  for (i in seq_len(nrow(m))) {
    expect_match(html, htmltools::htmlEscape(m$target[i]), fixed = TRUE)
    expect_match(html, htmltools::htmlEscape(m$sample[i]), fixed = TRUE)
  }
})

test_that("the table is ordered by the researcher's choice through a hidden column, with header sorting off", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  r <- hs_complete_search()$complete
  expect_identical(nrow(r$table), 8L)

  w <- sieve_candidate_widget(r)$x
  expect_identical(names(w$data), display_columns())
  # What a reader needs first is in the first columns: the order, the item
  # set, what it drops, and its status and reason.
  expect_identical(
    names(w$data)[1:6],
    c("position", "order", "id", "n_items", "dropped", "status")
  )
  # No ordering chosen: search order, reference first.
  expect_identical(w$data$position, 1:8)
  expect_identical(w$data$order, c("reference", rep("search order", 7)))
  expect_identical(w$data$id, as.character(1:8))
  # The initial order is the hidden position column, and no header sorts.
  expect_identical(w$options$order, list(list(0, "asc")))
  expect_identical(w$options$columnDefs[[1]], list(visible = FALSE, targets = 0))
  expect_identical(w$options$columnDefs[[2]], list(orderable = FALSE, targets = "_all"))
  expect_identical(w$selection$mode, "single")
  header <- as.character(w$container)
  # The coefficient is named "Coefficient Omega" with its symbol, and the
  # Optimizer ranks on robust omega, labeled as such.
  expect_match(header, "Reliability: Coefficient Omega, \u03c9, as Robust omega (observed denominator), one-factor model of the item codes treated as",
    fixed = TRUE
  )
  expect_match(header, "Fit of each item set", fixed = TRUE)
  expect_match(header, "every row with an observed kept item", fixed = TRUE)
  expect_match(header, "Simple structure: exploratory ML factor analysis", fixed = TRUE)
  expect_false(grepl("AIC", header, fixed = TRUE))
  expect_identical(length(sieve_candidate_widths()), ncol(w$data) - 1L)

  # Ordered by min omega: seven of eight have none. They are listed after
  # the reference, in search order, marked not ranked. No fallback to CFI.
  by_omega <- sieve_candidate_widget(r, "omega_min")$x$data
  expect_identical(by_omega$position, 1:8)
  expect_match(by_omega$omega_min[1], "^0\\.[0-9]{3}$")
  expect_identical(by_omega$order, c(
    sprintf("reference (%s)", by_omega$omega_min[1]),
    rep("not ranked", 7)
  ))

  # Ordered by CFI, because the researcher asked. The reference is still
  # first, the ranked rows are in decreasing CFI, and every row that is not
  # ranked follows them. In this example the two largest CFI values belong
  # to improper solutions (a negative variance estimate) and one model did
  # not converge: none of the three is ranked, the first two are shown with
  # a mark, and the third reads "unavailable" instead of being blank.
  by_cfi <- sieve_candidate_widget(r, "cfi")$x$data
  expect_false(identical(by_cfi$position, by_omega$position))
  expect_identical(by_cfi$position[1], 1L)
  # A ranked row reads "rank (value)"; a row that is not ranked reads
  # "not ranked", with its value and the mark when it has one.
  ranked <- grepl("^[0-9]+ \\(", by_cfi$order)
  held_back <- grepl("^not ranked", by_cfi$order)
  expect_identical(sum(ranked) + sum(held_back), 7L)
  expect_identical(
    by_cfi$order[ranked],
    sprintf(
      "%d (%s)", rank(-r$table$cfi[ranked], ties.method = "min"),
      by_cfi$cfi[ranked]
    )
  )
  expect_identical(
    by_cfi$order[held_back],
    ifelse(by_cfi$cfi[held_back] == "unavailable", "not ranked",
      sprintf("not ranked (%s)", by_cfi$cfi[held_back])
    )
  )
  expect_match(as.character(sieve_candidate_widget(r, "cfi")$x$container), "order (CFI)",
    fixed = TRUE
  )
  expect_true(all(grepl("^[01]\\.[0-9]{3}$", by_cfi$cfi[ranked])))
  expect_false(is.unsorted(rev(r$table$cfi[ranked][order(by_cfi$position[ranked])])))
  expect_true(max(by_cfi$position[ranked]) < min(by_cfi$position[held_back]))
  expect_true(all(grepl(" \\*$", by_cfi$cfi[held_back]) | by_cfi$cfi[held_back] == "unavailable"))
  expect_identical(
    sort(r$table$fit_code[held_back]),
    c("inadmissible", "inadmissible", "inadmissible", "not_converged")
  )
  expect_identical(by_cfi$cfi[r$table$fit_code == "not_converged"], "unavailable")
  expect_gt(max(r$table$cfi[held_back], na.rm = TRUE), max(r$table$cfi[ranked]))
})

test_that("the module explains unavailable omega and keeps the order the researcher chose", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  h <- hs_complete_search()
  r <- h$complete
  st <- optimizer_state()
  # The start of the table payload: the hidden position column, then the
  # order labels, in the search order of the rows.
  payload <- function(by) {
    d <- sieve_candidate_display(r, by)
    sprintf(
      "\"data\":[[%s],[%s],", paste(d$position, collapse = ","),
      paste(sprintf("\"%s\"", d$order), collapse = ",")
    )
  }
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(
      max_drop = 1, min_keep = 2, ss = FALSE, cap = 200,
      must_keep = h$protected, undersized = "hold"
    )
    expect_match(output$count$html, "8 candidate item sets", fixed = TRUE)
    expect_match(output$count$html, "Omega cannot be computed for those", fixed = TRUE)
    session$setInputs(run = 1)

    msg <- output$run_msg$html
    # The line that says what was searched sits above the table (run_head);
    # the notes on the table are under it.
    expect_match(output$run_head$html, "Examined all 8 item sets", fixed = TRUE)
    expect_match(output$run_head$html, "none was skipped", fixed = TRUE)
    expect_match(msg, "Omega (min and mean) is unavailable for 7 of 8 item sets", fixed = TRUE)
    expect_match(msg, "expected, not an error", fixed = TRUE)
    expect_match(msg, "not reordered to another statistic", fixed = TRUE)
    expect_match(msg, "Evaluate the item set you choose on independent data", fixed = TRUE)

    # No ordering chosen: search order, reference first.
    expect_match(output$results, payload("search"), fixed = TRUE)
    expect_match(output$results, "\"data\":[[1,2,3,4,5,6,7,8],[\"reference\",\"search order\"",
      fixed = TRUE
    )
    expect_match(output$results, "\"order\":[[0,\"asc\"]]", fixed = TRUE)
    expect_match(output$results, "\"visible\":false,\"targets\":0", fixed = TRUE)
    expect_match(output$results, "\"orderable\":false,\"targets\":\"_all\"", fixed = TRUE)

    # The researcher's choice is what reaches the table: min omega leaves
    # the rows in search order, marked not ranked; CFI reorders them.
    session$setInputs(order_by = "omega_min")
    expect_match(output$results, payload("omega_min"), fixed = TRUE)
    expect_match(output$results,
      sprintf(
        "[\"reference (%s)\",\"not ranked\",\"not ranked\"",
        sieve_candidate_display(r)$omega_min[1]
      ),
      fixed = TRUE
    )
    session$setInputs(order_by = "cfi")
    expect_match(output$results, payload("cfi"), fixed = TRUE)
    expect_false(identical(payload("cfi"), payload("omega_min")))

    # The detail pane spells out a selected row.
    expect_match(output$detail$html, "Select a row", fixed = TRUE)
    session$setInputs(results_rows_selected = 2)
    expect_match(output$detail$html, "Item set 2", fixed = TRUE)
    expect_match(output$detail$html, "omega unavailable", fixed = TRUE)
  })

  # The reproducible code log states every constraint and claims no choice.
  log <- paste(shiny::isolate(st$codelog), collapse = "\n")
  expect_match(log, "Optimizer: searched 8 item-sets, all examined", fixed = TRUE)
  expect_match(log, "always keep t2_cubes", fixed = TRUE)
  expect_match(log, "the search chose and applied nothing", fixed = TRUE)
  expect_false(grepl("best chosen", log, fixed = TRUE))

  # A refused search shows the reason, and the table does not render stale rows.
  st2 <- optimizer_state()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st2$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st2), {
    session$setInputs(max_drop = 1, min_keep = 2, ss = FALSE, cap = 5, undersized = "hold")
    expect_match(output$count$html, "above the cap of 5", fixed = TRUE)
    session$setInputs(run = 1)
    expect_match(output$run_msg$html, "no candidate was built or fit", fixed = TRUE)
    expect_match(output$run_msg$html, "64 candidate item sets, above the cap of 5", fixed = TRUE)
    # The table is withheld by req(), which signals shiny's silent error.
    expect_error(output$results, class = "shiny.silent.error")
  })
})

test_that("a held facet is reported wherever a held construct is", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  st <- optimizer_state()
  meta <- shiny::isolate(st$meta)
  # Facet v2 of Visual has one item; with a facet minimum of two it is held.
  meta$facet <- c("v1", "v1", "v2", "", "", "", "s1", "s1", "s1")
  st$meta <- meta
  lone <- meta$item[3]
  named <- "Facet v2 of Visual is held: too few items to exclude any (1 item, facet minimum 2)."
  expect_identical(
    sieve_held_text(NULL, data.frame(
      construct = "Visual", facet = "v2", n_items = 1L,
      min_facet = 2L, n_must_keep = 0L, held = TRUE
    )),
    named
  )
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(
      max_drop = 1, min_keep = 2, min_facet = 2, ss = FALSE, cap = 300,
      undersized = "hold"
    )
    expect_match(panel_text(output$count$html), named, fixed = TRUE)
    expect_match(panel_text(output$count$html), "All of its items stay in every candidate",
      fixed = TRUE
    )
    session$setInputs(run = 1)
    r <- res()
    expect_null(r$error)
    expect_identical(r$facets$facet[r$facets$held], "v2")
    # Held means what it says: the facet's item is in every candidate.
    expect_true(all(vapply(r$kept, function(k) lone %in% k, logical(1))))
    expect_match(panel_text(output$run_msg$html), named, fixed = TRUE)
    # The constraints searched are named in the line above the table
    # (run_head); the notes are under it.
    expect_match(panel_text(output$run_head$html),
      "a construct or facet below its minimum is held intact",
      fixed = TRUE
    )
    expect_match(paste(r$notes, collapse = " "),
      "Facet v2 of Visual: held: too few items to exclude any (1 item, facet minimum 2)",
      fixed = TRUE
    )
    # The detail pane of any row, and the table itself (its caption), name
    # the held facet in the same words.
    session$setInputs(results_rows_selected = 1L)
    detail <- panel_text(output$detail$html)
    expect_match(detail, "Held intact in every item set:", fixed = TRUE)
    expect_match(detail, named, fixed = TRUE)
    expect_match(detail, "These items are in this item set and in every other", fixed = TRUE)
    expect_match(output$results, named, fixed = TRUE)
    expect_match(output$results, "All of its items are in every item set in this table", fixed = TRUE)
    expect_match(as.character(sieve_candidate_widget(r)$x$caption), named, fixed = TRUE)
  })
  log <- paste(shiny::isolate(st$codelog), collapse = "\n")
  expect_match(log, paste("# Held intact in every item set:", named), fixed = TRUE)

  # A held construct and a held facet together, in the same words.
  both <- ledger(c(rep("A", 4), rep("B", 2)), facet = c("f1", "f1", "f1", "f2", NA, NA))
  plan <- sieve_optimizer_plan(both, sieve_optimizer_constraints(
    max_drop = 1L, min_keep = 3L,
    min_facet = 2L
  ))
  expect_identical(sieve_held_text(plan$constructs, plan$facets), c(
    "B is held: too few items to exclude any (2 items, minimum 3).",
    "Facet f2 of A is held: too few items to exclude any (1 item, facet minimum 2)."
  ))
  expect_identical(sieve_held_text(
    plan$constructs[plan$constructs$state != "held", ],
    plan$facets[!plan$facets$held, ]
  ), character(0))
  # The table's caption carries both, and nothing when nothing is held.
  caption <- as.character(sieve_candidate_caption(
    list(constructs = plan$constructs, facets = plan$facets),
    fixed = list(note = NULL)
  ))
  expect_match(caption, "B is held: too few items to exclude any (2 items, minimum 3). Facet f2 of A is held",
    fixed = TRUE
  )
  expect_match(caption, "All of their items are in every item set in this table", fixed = TRUE)
  expect_null(sieve_candidate_caption(
    list(
      constructs = plan$constructs[plan$constructs$state != "held", ],
      facets = plan$facets[!plan$facets$held, ]
    ),
    fixed = list(note = NULL)
  ))
  # Without a facet minimum no facet is spoken of.
  expect_match(sieve_constraints_text(sieve_optimizer_constraints()),
    "a construct below its minimum is held intact",
    fixed = TRUE
  )
  expect_match(
    sieve_constraints_text(sieve_optimizer_constraints(
      min_facet = 2L,
      undersized = "refuse"
    )),
    "a construct or facet below its minimum stops the search",
    fixed = TRUE
  )
})

test_that("a statistic the search did not compute is not offered for ordering, and the status says so", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  h <- hs_complete_search()
  r <- h$complete
  expect_false(r$simple_structure)
  expect_true(hs_search()$out$simple_structure)

  # The ordering control: every statistic when simple structure was
  # computed, the reliability and fit statistics alone when it was not.
  all_choices <- unname(sieve_order_choices())
  expect_identical(all_choices, c("search", sieve_optimizer_metrics()$metric))
  expect_identical(
    unname(sieve_order_choices(structure = FALSE)),
    c("search", "omega_min", "omega_mean", "cfi", "rmsea", "srmr")
  )
  # Items are excluded, not dropped, in the panel's text.
  expect_identical(names(sieve_order_choices())[1], "Search order (fewest items excluded first)")
  expect_identical(sieve_order_by(r, "complexity"), "search")
  expect_identical(sieve_order_by(r, "hyperplane"), "search")
  expect_identical(sieve_order_by(r, "cfi"), "cfi")
  expect_identical(sieve_order_by(r, NULL), "search")
  expect_identical(sieve_order_by(r, "aic"), "search")
  expect_identical(sieve_order_by(hs_search()$out, "complexity"), "complexity")

  # What the control offers before any search follows the checkbox, so a
  # search with simple structure off never starts with complexity or the
  # hyperplane proportion selectable; after a search it follows what that
  # search computed, whatever the checkbox now says; after a refused search
  # there is no table, so the checkbox decides again.
  expect_true(sieve_order_structure(NULL, TRUE))
  expect_false(sieve_order_structure(NULL, FALSE))
  expect_true(sieve_order_structure(NULL, NULL))
  expect_true(sieve_order_structure(list(error = "refused"), TRUE))
  expect_false(sieve_order_structure(list(error = "refused"), FALSE))
  expect_true(sieve_order_structure(hs_search()$out, FALSE))
  expect_false(sieve_order_structure(r, TRUE))
  expect_identical(
    unname(sieve_order_choices(structure = sieve_order_structure(NULL, FALSE))),
    c("search", "omega_min", "omega_mean", "cfi", "rmsea", "srmr")
  )

  # The status and reason column says that simple structure was not
  # requested, on every row, so "not ranked" on complexity never stands
  # beside a status reading that every statistic is available.
  d <- sieve_candidate_display(r, "complexity")
  expect_true(all(grepl("simple structure not requested", d$status, fixed = TRUE)))
  expect_false(any(grepl("all statistics available", d$status, fixed = TRUE)))
  expect_identical(d$complexity, rep("not requested", 8L))
  expect_identical(d$order, c("reference", rep("not ranked", 7)))
  # When it was requested and computed, the plain wording stands.
  full <- sieve_candidate_display(hs_search()$out)
  expect_false(any(grepl("not requested", full$status, fixed = TRUE)))

  st <- optimizer_state()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(
      max_drop = 1, min_keep = 2, ss = FALSE, cap = 200,
      must_keep = h$protected, undersized = "hold", order_by = "complexity"
    )
    session$setInputs(run = 1)
    expect_match(panel_text(output$run_msg$html),
      "Simple structure was not requested for this search",
      fixed = TRUE
    )
    # The table is in search order and says so; nothing is ranked on a
    # statistic that does not exist, and no other statistic is substituted.
    expect_match(output$results,
      "\"data\":[[1,2,3,4,5,6,7,8],[\"reference\",\"search order\",\"search order\"",
      fixed = TRUE
    )
  })
})

test_that("the panel uses its settings exactly as typed, or refuses with the reason", {
  # Usable settings come back unchanged.
  ok <- sieve_optimizer_panel_settings(
    max_drop = 2, min_keep = 3, min_facet = NA,
    must_keep = c("i01", "i02"), undersized = "refuse",
    cap = 250
  )
  expect_null(ok$error)
  expect_identical(ok$constraints, sieve_optimizer_constraints(
    max_drop = 2L, min_keep = 3L, must_keep = c("i01", "i02"), undersized = "refuse"
  ))
  expect_identical(ok$cap, 250)
  expect_null(ok$min_facet)
  # Before the page has drawn a field its input is NULL: the field's own
  # initial value is used.
  fresh <- sieve_optimizer_panel_settings()
  expect_null(fresh$error)
  expect_identical(c(fresh$max_drop, fresh$min_keep, fresh$cap), c(1, 3, 300))
  expect_identical(fresh$undersized, "hold")
  expect_identical(sieve_optimizer_panel_limits(), list(min_keep = 2L, cap = 3000L))
  # A large maximum to drop is a usable value and is kept as typed.
  expect_identical(sieve_optimizer_panel_settings(max_drop = 1e6)$constraints$max_drop, 1000000L)

  refused <- list(
    list(arg = list(max_drop = -3), say = "must be a single whole number, zero or more"),
    list(arg = list(max_drop = 2.9), say = "must be a single whole number, zero or more"),
    list(arg = list(max_drop = NA_real_), say = "must be a single whole number, zero or more"),
    list(arg = list(max_drop = 1e12), say = "too large to be a number of items"),
    list(arg = list(min_keep = 1), say = "2 or more, in this panel (you entered 1)"),
    list(arg = list(min_keep = 0), say = "2 or more, in this panel (you entered 0)"),
    list(arg = list(min_keep = 2.5), say = "must be a whole number, 2 or more, in this panel (you entered 2.5)"),
    list(arg = list(min_keep = NA_real_), say = "in this panel (the field is empty)"),
    list(arg = list(min_facet = -1), say = "minimum number of items to keep per facet"),
    list(arg = list(min_facet = 1.5), say = "minimum number of items to keep per facet"),
    list(arg = list(cap = 0), say = "The evaluation cap must be a whole number, one or more (you entered 0)"),
    list(arg = list(cap = 2.5), say = "The evaluation cap must be a whole number, one or more (you entered 2.5)"),
    list(
      arg = list(cap = NA_real_),
      say = "The evaluation cap must be a whole number, one or more (the field is empty)"
    ),
    list(arg = list(cap = 5000), say = "The evaluation cap can be at most 3,000 in this panel (you entered 5,000)"),
    list(arg = list(cap = 1e12), say = "at most 3,000 in this panel (you entered 1,000,000,000,000)")
  )
  for (case in refused) {
    # No value is coerced, so nothing warns.
    got <- expect_no_warning(do.call(sieve_optimizer_panel_settings, case$arg))
    expect_match(got$error, case$say, fixed = TRUE)
    # The value is reported back as typed, never replaced.
    expect_identical(got[[names(case$arg)]], case$arg[[1]])
  }
  expect_match(sieve_optimizer_panel_settings(min_keep = 1)$error,
    "A construct left with one item cannot define a factor",
    fixed = TRUE
  )

  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  st <- optimizer_state()
  before <- shiny::isolate(st$meta$status)
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    base <- list(
      max_drop = 0, min_keep = 3, min_facet = NA, ss = FALSE, cap = 300,
      undersized = "hold"
    )
    n_run <- 0L
    for (case in refused) {
      typed <- base
      typed[names(case$arg)] <- case$arg
      n_run <- n_run + 1L
      expect_no_warning(do.call(session$setInputs, typed))
      # The live count carries the reason where the number was typed ...
      expect_match(panel_text(expect_no_warning(output$count$html)), case$say, fixed = TRUE)
      # ... and running does not search some other setting instead.
      expect_no_warning(session$setInputs(run = n_run))
      r <- res()
      expect_match(r$error, "The search was not run.", fixed = TRUE)
      expect_match(r$error, case$say, fixed = TRUE)
      expect_identical(r$evaluated, 0L)
      expect_null(r$table)
      expect_null(r$kept)
      expect_match(panel_text(output$run_msg$html), case$say, fixed = TRUE)
    }
    # The same panel, with usable values, searches them as typed.
    do.call(session$setInputs, base)
    session$setInputs(run = n_run + 1L)
    r <- res()
    expect_null(r$error)
    expect_identical(r$constraints$max_drop, 0L)
    expect_identical(r$constraints$min_keep, 3L)
    expect_identical(nrow(r$table), 1L)
  })
  expect_identical(shiny::isolate(st$meta$status), before)
  # Only the one search that ran is in the code log.
  expect_identical(sum(grepl("Optimizer: searched", shiny::isolate(st$codelog), fixed = TRUE)), 1L)
})

test_that("a typed maximum far above the pool is counted and refused at once in the module", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  fx <- sieve_hs_fixture()
  items <- fx$data[, fx$item_cols, drop = FALSE]
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  st <- plain_state(items, meta)
  # As above: a failure stops here instead of planning a million times
  # or building a quarter of a million item sets.
  plan_once <- sieve_optimizer_plan
  calls <- new.env()
  calls$n <- 0L
  testthat::local_mocked_bindings(
    sieve_optimizer_plan = function(...) {
      calls$n <- calls$n + 1L
      if (calls$n > 4L) stop("one request planned the search more than four times")
      plan_once(...)
    },
    sieve_enumerate_candidates = function(...) stop("the enumerator was called"),
    sieve_construct_drop_sets = function(...) stop("the enumerator was called"),
    sieve_combn = function(...) stop("combinations were built"),
    .package = "SIEVER"
  )
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    seen <- character(0)
    for (max_drop in c(4, 1000, 1e6)) {
      session$setInputs(
        max_drop = max_drop, min_keep = 3, ss = FALSE, cap = 300,
        undersized = "hold"
      )
      counted <- panel_text(output$count$html)
      calls$n <- 0L
      session$setInputs(run = max_drop)
      # One plan to size the progress bar and one in the search, whatever
      # the number typed (not one per unit of the maximum).
      expect_identical(calls$n, 2L)
      expect_match(res()$error, "no candidate was built or fit", fixed = TRUE)
      expect_match(res()$error, "268,800 candidate item sets, above the cap of 300", fixed = TRUE)
      expect_lte(nrow(res()$alternatives), max(table(meta$construct)))
      expect_match(counted, "268,800 candidate item sets", fixed = TRUE)
      # Items are excluded, not dropped, in the panel's text.
      expect_match(counted, sprintf(
        "a maximum of %s allows the same exclusions as 3",
        format(max_drop, big.mark = ",", scientific = FALSE)
      ),
      fixed = TRUE
      )
      seen <- c(seen, res()$error)
    }
    # The three typed values amount to the same search and the same refusal.
    expect_identical(length(unique(seen)), 1L)
  })
})

# ---------------------------------------------------------------------------
# A refusal's cost, frozen columns, a stale selection, and what the panel
# says before, beside, and after a search
# ---------------------------------------------------------------------------

test_that("a refusal above the cap is immediate whatever maximum to drop is typed, and a count beyond 2^53 is called approximate in the panel", {
  big <- ledger(rep("F", 60))
  data <- as.data.frame(matrix(0, 5, 60, dimnames = list(NULL, big$item)))
  # The work is bounded by the pool (57 items can be dropped from a
  # construct of 60 with a minimum of 3), not by the typed number. A
  # refusal that planned once per unit of the maximum, about 0.8 ms each,
  # would take 23 s at 30,000 and about 770 s at a million, so five seconds,
  # generous for a loaded machine, is two orders of magnitude below that at
  # a million and a thousand below it at the integer limit. The mocked
  # count of plans above is the structural guarantee; this is the wall
  # clock, checked when SIEVER_TIMING_TESTS is true.
  took <- vapply(c(30, 1e4, 1e6, .Machine$integer.max), function(max_drop) {
    t <- system.time(
      r <- sieve_optimize_items(data, big, max_drop = max_drop, min_keep = 3L, cap = 300L)
    )[["elapsed"]]
    expect_match(r$error, "no candidate was built or fit", fixed = TRUE)
    expect_identical(r$evaluated, 0L)
    expect_lte(nrow(r$alternatives), 57L)
    t
  }, numeric(1))
  if (siever_timing_checks()) expect_true(all(took < 5))

  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  st <- plain_state(as.data.frame(matrix(0, 3, 60, dimnames = list(NULL, big$item))), big)
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    for (max_drop in c(30, 1e6, .Machine$integer.max)) {
      session$setInputs(
        max_drop = max_drop, min_keep = 3, ss = FALSE, cap = 300,
        undersized = "hold"
      )
      t <- system.time({
        counted <- panel_text(output$count$html)
        session$setInputs(run = max_drop)
      })[["elapsed"]]
      if (siever_timing_checks()) expect_lt(t, 5)
      expect_match(res()$error, "no candidate was built or fit", fixed = TRUE)
      # The count is above 2^53 (sum of C(60, j) up to 30 is about 6.36e17;
      # up to 57, the most the construct can lose, about 1.15e18), so no
      # double holds it exactly: the panel says about, and never exact,
      # wherever it states it.
      about <- if (max_drop == 30) "about 6.36e+17" else "about 1.15e+18"
      expect_match(counted, sprintf("%s candidate item sets", about), fixed = TRUE)
      expect_match(counted, "above the cap of 300, so Run search would be refused", fixed = TRUE)
      expect_false(grepl("exact", counted, fixed = TRUE))
      expect_match(res()$error, sprintf("define %s candidate item sets", about), fixed = TRUE)
      expect_false(grepl("exact", res()$error, fixed = TRUE))
      expect_match(panel_text(output$run_msg$html), "stated approximately", fixed = TRUE)
    }
  })
})

test_that("the first columns of the candidate table are frozen when DT can freeze them, and the table says so when it cannot", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  h <- hs_complete_search()
  r <- h$complete
  # The first four drawn columns: the order with the value it is ordered
  # by, the id, the number of items kept, and the dropped items. The hidden
  # position column is not counted: FixedColumns counts drawn columns.
  fixed <- sieve_candidate_fixed_columns()
  expect_identical(fixed$available, sieve_dt_has_extension("FixedColumns"))
  expect_identical(names(sieve_candidate_display(r))[2:5], c("order", "id", "n_items", "dropped"))
  if (fixed$available) {
    expect_identical(fixed$extensions, "FixedColumns")
    expect_identical(fixed$options, list(fixedColumns = list(leftColumns = 4L)))
    expect_null(fixed$note)
    w <- sieve_candidate_widget(r, "cfi")$x
    expect_identical(unlist(w$extensions), "FixedColumns")
    expect_identical(w$options$fixedColumns$leftColumns, 4L)
    expect_null(w$caption)
    expect_identical(w$data$order[1], sprintf("reference (%s)", w$data$cfi[1]))
  }
  # Without the extension (a repackaged DT leaves bundled extensions out):
  # drawn without it and without an option that would make DT stop, and
  # the caption says what is missing and how to restore it.
  testthat::local_mocked_bindings(
    sieve_dt_has_extension = function(extension) FALSE,
    .package = "SIEVER"
  )
  without <- sieve_candidate_fixed_columns()
  expect_false(without$available)
  expect_identical(without$extensions, list())
  expect_identical(without$options, list())
  expect_match(without$note, "does not include the DataTables FixedColumns extension", fixed = TRUE)
  expect_match(without$note, "install.packages(\"DT\")", fixed = TRUE)
  w <- sieve_candidate_widget(r, "cfi")$x
  expect_length(unlist(w$extensions), 0L)
  expect_null(w$options$fixedColumns)
  expect_match(as.character(w$caption), "scroll with the rest of the table", fixed = TRUE)
  st <- optimizer_state()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(
      max_drop = 1, min_keep = 2, ss = FALSE, cap = 200, must_keep = h$protected,
      undersized = "hold"
    )
    session$setInputs(run = 1)
    expect_match(output$results, "does not include the DataTables FixedColumns extension",
      fixed = TRUE
    )
    expect_false(grepl("fixedColumns", output$results, fixed = TRUE))
  })
})

test_that("a selection beyond the table, or an item set outside the search's constraints, is refused by Apply without an error", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  st <- optimizer_state()
  before <- shiny::isolate(st$meta$status)
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(max_drop = 0, min_keep = 3, ss = FALSE, cap = 300, undersized = "hold")
    session$setInputs(run = 1)
    expect_identical(nrow(res()$table), 1L)
    # A selection index the table does not have (row 99 of a one row table)
    # is handled: the Apply observer raises no subscript error, and the
    # session stays open.
    session$setInputs(results_rows_selected = 99L)
    expect_match(panel_text(output$detail$html), "Select a row", fixed = TRUE)
    expect_no_error(session$setInputs(apply = 1L))
    expect_identical(state$meta$status, before)
    expect_false(session$isClosed())
    expect_null(applied())
    expect_false(grepl("Applied item set", panel_text(output$run_msg$html), fixed = TRUE))
    # An item set that does not satisfy the constraints the search was run
    # with (here the result altered to keep two items) is refused by the
    # same check the engine applied before returning its candidates.
    r <- res()
    r$kept[[1]] <- r$kept[[1]][1:2]
    res(r)
    session$setInputs(results_rows_selected = 1L)
    expect_no_error(session$setInputs(apply = 2L))
    expect_identical(state$meta$status, before)
    expect_null(applied())
  })
})

test_that("the count panel is one paragraph: what the settings define, what is held, the cap, and what Run search would do", {
  expect_identical(sieve_rough_seconds(0.4), "about a second")
  expect_identical(sieve_rough_seconds(1.2), "about a second")
  expect_identical(sieve_rough_seconds(40.4), "about 40 seconds")
  expect_identical(sieve_rough_seconds(89), "about 89 seconds")
  expect_identical(sieve_rough_seconds(100), "about 2 minutes")
  expect_identical(sieve_rough_seconds(3600), "about 60 minutes")
  expect_identical(sieve_rough_seconds(7200), "about 2.0 hours")
  expect_identical(sieve_rough_seconds(Inf), "an unknown time")
  expect_identical(sieve_rough_seconds(NA_real_), "an unknown time")
  # The forecast scales the last search's rate to the new count, and says
  # when the simple structure setting has changed since.
  last <- list(error = NULL, elapsed = 10, evaluated = 5L, simple_structure = TRUE)
  f <- sieve_run_forecast(20, "20", simple_structure = TRUE, last = last)
  expect_match(f, "Run search examines all 20 while the session waits", fixed = TRUE)
  expect_match(f, "and one exploratory factor analysis for simple structure", fixed = TRUE)
  expect_match(f, paste(
    "The last search examined 5 item sets in about 10 seconds with simple",
    "structure, so this one would take about 40 seconds."
  ), fixed = TRUE)
  expect_match(sieve_run_forecast(20, "20", FALSE, last),
    "about 40 seconds or less, since simple structure is now left out",
    fixed = TRUE
  )
  expect_match(
    sieve_run_forecast(20, "20", TRUE, list(
      error = NULL, elapsed = 10, evaluated = 5L,
      simple_structure = FALSE
    )),
    "or more, since simple structure is now included",
    fixed = TRUE
  )
  expect_identical(sieve_run_forecast(1, "1", FALSE, NULL), paste(
    "Run search describes your current item set alone (its correlated factors model and a",
    "one-factor model per construct for omega). There is no earlier search in this session to",
    "estimate the time from."
  ))
  expect_match(sieve_run_forecast(20, "20", TRUE, list(error = "refused", total = 0, evaluated = 0L)),
    "no earlier search",
    fixed = TRUE
  )

  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  st <- optimizer_state()
  meta <- shiny::isolate(st$meta)
  meta$facet <- c("v1", "v1", "v2", "", "", "", "s1", "s1", "s1")
  st$meta <- meta
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(
      max_drop = 1, min_keep = 2, min_facet = 2, ss = FALSE, cap = 300,
      undersized = "hold"
    )
    html <- output$count$html
    # One block, read as a paragraph, not a stack of alerts.
    expect_identical(lengths(regmatches(html, gregexpr("<div", html, fixed = TRUE))), 1L)
    text <- panel_text(html)
    expect_match(text, paste0(
      "^These settings define 16 candidate item sets, counting your ",
      "current item set, within the cap of 300\\. "
    ))
    expect_match(text, paste(
      "Facet v2 of Visual is held: too few items to exclude any (1 item, facet",
      "minimum 2). All of its items stay in every candidate."
    ), fixed = TRUE)
    expect_match(text, paste(
      "With a minimum of 2, some candidates leave a construct with fewer than",
      "3 items. Omega cannot be computed for those"
    ), fixed = TRUE)
    expect_match(text, paste(
      "Run search examines all 16 while the session waits, fitting for each",
      "item set its correlated factors model and a one-factor model per",
      "construct for omega. There is no earlier search in this session to",
      "estimate the time from."
    ), fixed = TRUE)
    expect_match(html, "alert-info", fixed = TRUE)
    # Above the cap: the same paragraph says so, and there is no forecast.
    session$setInputs(cap = 5)
    text <- panel_text(output$count$html)
    expect_match(text, "above the cap of 5, so Run search would be refused before anything is built or fit",
      fixed = TRUE
    )
    expect_false(grepl("Run search examines", text, fixed = TRUE))
    expect_match(output$count$html, "alert-warning", fixed = TRUE)
    # A typed maximum more than any construct can lose: said, used as typed.
    session$setInputs(cap = 300, max_drop = 1000)
    # Items are excluded, not dropped, in the panel's text.
    expect_match(panel_text(output$count$html),
      paste(
        "No construct can lose more than 1 item under these constraints, so a",
        "maximum of 1,000 allows the same exclusions as 1."
      ),
      fixed = TRUE
    )
    # After a search, the next one is forecast from that search's rate, and
    # the forecast follows the simple structure checkbox.
    session$setInputs(max_drop = 1)
    session$setInputs(run = 1)
    r <- res()
    expect_true(is.numeric(r$elapsed) && is.finite(r$elapsed) && r$elapsed > 0)
    expect_match(panel_text(output$count$html), paste0(
      "The last search examined 16 item sets in about (a second|[0-9]+ seconds|[0-9]+ minutes?) ",
      "without simple structure, so this one would take about "
    ))
    session$setInputs(ss = TRUE)
    expect_match(panel_text(output$count$html), "or more, since simple structure is now included",
      fixed = TRUE
    )
    session$setInputs(ss = FALSE, max_drop = 0)
    text <- panel_text(output$count$html)
    expect_match(text,
      "These settings define 1 candidate item set, counting your current item set, within the cap of 300.",
      fixed = TRUE
    )
    expect_match(text, "Run search describes your current item set alone", fixed = TRUE)
  })
})

test_that("the detail pane says in sentences where a candidate stands in the chosen order, why, and which statistics are unavailable", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  h <- hs_complete_search()
  r <- h$complete
  ord <- sieve_order_candidates(r$table, "cfi")
  ranked <- which(ord$group == "ranked")
  not_ranked <- which(ord$group == "not ranked")
  expect_gt(length(ranked), 0L)
  expect_gt(length(not_ranked), 0L)
  said <- function(row, by) panel_text(as.character(sieve_candidate_detail(r, row, by = by)))
  value <- function(row) formatC(r$table$cfi[row], format = "f", digits = 3)

  # The reference, ordered by CFI.
  expect_match(said(1, "cfi"), sprintf(paste(
    "Order: Ordered by CFI (highest first): this is your current item set, the reference, shown",
    "first and not ranked among the candidates; its CFI is %s."
  ), value(1)), fixed = TRUE)
  # A ranked candidate: its rank among those that can be ranked, its
  # position in the table, and its value.
  i <- ranked[1]
  expect_match(said(i, "cfi"), sprintf(
    paste(
      "Order: Ordered by CFI (highest first): ranked %d of the %d item sets that can be ranked on",
      "it, at position %d of 8; its CFI is %s."
    ), ord$rank[i], length(ranked), ord$position[i],
    value(i)
  ), fixed = TRUE)
  # One that is not ranked: after the ranked ones, with the fit family's
  # reason, and its value marked when it has one.
  j <- not_ranked[1]
  text <- said(j, "cfi")
  expect_match(text, sprintf(paste(
    "Order: Ordered by CFI (highest first): not ranked, listed after the %d ranked item sets at",
    "position %d of 8, because %s;"
  ), length(ranked), ord$position[j], r$table$fit_note[j]),
  fixed = TRUE
  )
  if (is.finite(r$table$cfi[j])) {
    expect_match(text, sprintf(paste(
      "its CFI is %s, shown with an asterisk because it is computed",
      "but not usable for ordering."
    ), value(j)), fixed = TRUE)
  } else {
    expect_match(text, "no CFI is available for it.", fixed = TRUE)
  }
  expect_match(said(1, "rmsea"), "Ordered by RMSEA (lowest first)", fixed = TRUE)
  # Search order ranks nothing, and says so.
  expect_match(said(1, "search"), paste(
    "Order: The table is in search order; this is your current item set, shown first as the",
    "reference."
  ), fixed = TRUE)
  # Items are excluded, not dropped, in the panel's text.
  expect_match(said(2, "search"), paste(
    "Order: The table is in search order (fewest items excluded first), which ranks nothing;",
    "this item set excludes 1 item and is listed at position 2 of 8."
  ), fixed = TRUE)
  # Status, family by family, with the reason for whatever is unavailable.
  expect_match(said(1, "search"), paste(
    "Status: Eligible: every requested statistic is available and usable for ordering.",
    "Reliability: available and usable for ordering. Fit: available and usable for ordering.",
    "Simple structure: not requested for this search, so it is neither shown nor offered for",
    "ordering."
  ), fixed = TRUE)
  partly <- which(r$table$status == "partly eligible")[1]
  expect_match(said(partly, "search"), paste(
    "Status: Partly eligible: some statistics are unavailable or not ranked. Reliability: omega",
    "unavailable ("
  ), fixed = TRUE)
  none <- which(r$table$status == "not eligible")[1]
  expect_match(said(none, "search"), "Status: Not eligible: no statistic can be ranked. Reliability: omega unavailable",
    fixed = TRUE
  )
  expect_match(said(none, "search"), sprintf("Fit: %s.", r$table$fit_note[none]), fixed = TRUE)
  # With simple structure computed, the third family reads like the others
  # and can order the table.
  full <- hs_search()$out
  text <- panel_text(as.character(sieve_candidate_detail(full, 1, by = "complexity")))
  expect_match(text, "Ordered by complexity (lowest first): this is your current item set", fixed = TRUE)
  expect_match(text, "Simple structure: available and usable for ordering.", fixed = TRUE)
  # Nothing is held in these searches, so the pane does not speak of it.
  expect_false(grepl("Held intact", text, fixed = TRUE))
  expect_false(grepl("Held intact", said(1, "search"), fixed = TRUE))

  # The module hands the order in force to the pane.
  st <- optimizer_state()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(
      max_drop = 1, min_keep = 2, ss = FALSE, cap = 200, must_keep = h$protected,
      undersized = "hold", order_by = "cfi"
    )
    session$setInputs(run = 1)
    session$setInputs(results_rows_selected = i)
    expect_match(panel_text(output$detail$html), "Ordered by CFI (highest first): ranked", fixed = TRUE)
    session$setInputs(order_by = "search")
    # Items are excluded, not dropped, in the panel's text.
    expect_match(panel_text(output$detail$html), "The table is in search order (fewest items excluded first)",
      fixed = TRUE
    )
    # A statistic this search did not compute: search order, and said so.
    session$setInputs(order_by = "complexity")
    expect_match(panel_text(output$detail$html), "The table is in search order", fixed = TRUE)
  })
})

test_that("after an Apply the message names the excluded items and offers Undo, which puts the ledger back exactly", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  h <- hs_complete_search()
  st <- optimizer_state()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(
      max_drop = 1, min_keep = 2, ss = FALSE, cap = 200, must_keep = h$protected,
      undersized = "hold"
    )
    session$setInputs(run = 1)
    r <- res()
    start <- state$meta$status
    expect_false(grepl("Applied item set", panel_text(output$run_msg$html), fixed = TRUE))

    # Apply the second row: one item excluded, named, with Undo beside it.
    session$setInputs(results_rows_selected = 2L, apply = 1L)
    dropped <- setdiff(r$pool, r$kept[[2]])
    expect_length(dropped, 1L)
    expect_identical(state$meta$status[state$meta$item == dropped], "parked")
    done <- applied()
    expect_identical(as.integer(done$id), 2L)
    expect_identical(done$dropped, dropped)
    expect_identical(done$returned, character(0))
    expect_identical(done$before$status, start)
    msg <- panel_text(output$run_msg$html)
    expect_match(msg, sprintf(
      "Applied item set 2. Excluded 1 item: %s. 8 items are now included.",
      dropped
    ), fixed = TRUE)
    expect_match(msg, "Undo this Apply", fixed = TRUE)
    expect_match(msg, "Undo puts the ledger back exactly as it was before this Apply", fixed = TRUE)
    expect_match(output$run_msg$html, "undo\"", fixed = TRUE)
    expect_null(stale())
    after_one <- state$meta$status

    # A second choice from the same search: the message says what it
    # excluded and what it included again, and Undo returns to the ledger
    # the first choice left, not to the start.
    session$setInputs(results_rows_selected = 3L, apply = 2L)
    done <- applied()
    expect_identical(done$before$status, after_one)
    expect_identical(done$returned, dropped)
    expect_identical(done$dropped, setdiff(r$kept[[2]], r$kept[[3]]))
    msg <- panel_text(output$run_msg$html)
    expect_match(msg, sprintf("Applied item set 3. Excluded 1 item: %s.", done$dropped), fixed = TRUE)
    expect_match(msg, sprintf(paste(
      "Included again 1 item that an earlier choice from this search",
      "had excluded: %s."
    ), dropped), fixed = TRUE)
    session$setInputs(undo = 1L)
    expect_identical(state$meta$status, after_one)
    expect_null(applied())
    expect_null(stale())
    expect_false(grepl("Applied item set", panel_text(output$run_msg$html), fixed = TRUE))
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "Optimizer: undid the applied item-set", fixed = TRUE)
    expect_match(log, sprintf("meta$status[meta$item %%in%% c(\"%s\")] <- \"active\"", done$dropped), fixed = TRUE)
    expect_match(log, sprintf("meta$status[meta$item %%in%% c(\"%s\")] <- \"parked\"", dropped), fixed = TRUE)
    expect_false(grepl("items <- setdiff(items", log, fixed = TRUE))
    # Nothing left to undo: pressing again changes nothing.
    session$setInputs(undo = 2L)
    expect_identical(state$meta$status, after_one)

    # Undo is withdrawn once the ledger has changed elsewhere, and refused
    # if pressed: a ledger changed since is never written over.
    session$setInputs(results_rows_selected = 2L, apply = 3L)
    expect_match(panel_text(output$run_msg$html), "Undo this Apply", fixed = TRUE)
    state$meta$status[state$meta$item == r$kept[[2]][1]] <- "parked"
    session$flushReact()
    msg <- panel_text(output$run_msg$html)
    expect_match(msg, "cannot be undone from here", fixed = TRUE)
    expect_false(grepl("Undo this Apply", msg, fixed = TRUE))
    changed <- state$meta$status
    session$setInputs(undo = 3L)
    expect_identical(state$meta$status, changed)
    expect_false(is.null(applied()))
  })
})

# ---------------------------------------------------------------------------
# The browser (under the supported environment)
# ---------------------------------------------------------------------------

test_that("in the browser, unavailable omega is explained, no cell is blank, and the chosen order is kept", {
  skip_if_no_browser()
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")

  # Every row of the table, in the order shown, as the browser holds it.
  shown_rows <- function(app) {
    app$get_js(paste0(
      "$('#optimize-results .dataTables_scrollBody table').DataTable()",
      ".rows({order: 'applied'}).data().toArray()"
    ))
  }
  column <- function(rows, j) vapply(rows, function(r) as.character(r[[j]]), character(1))
  # DT selects a row on mouse down, so a bare click() is not enough.
  press_row <- function(app, k) {
    app$run_js(sprintf(paste0(
      "(function(){var el = document.querySelector('#optimize-results ",
      ".dataTables_scrollBody table tbody tr:nth-child(%d) td:nth-child(2)');",
      "['mousedown','mouseup','click'].forEach(function(t){",
      "el.dispatchEvent(new MouseEvent(t, {bubbles: true, cancelable: true, view: window}));});",
      "})()"
    ), k))
    siever_wait_render(app)
  }

  # Which cells of the first row lie wholly inside the table's viewport, so
  # they are read without scrolling sideways (drawn columns, in order).
  on_screen <- function(app) {
    unlist(app$get_js(paste0(
      "(function(){var b = document.querySelector('#optimize-results .dataTables_scrollBody');",
      "var r = b.getBoundingClientRect(); var out = [];",
      "b.querySelectorAll('table tbody tr:first-child td').forEach(function(td){",
      "var q = td.getBoundingClientRect(); out.push(q.left >= r.left - 1 && q.right <= r.right + 1);});",
      "return out;})()"
    )))
  }
  cols <- display_columns()
  at <- function(name) match(name, cols)
  drawn_cols <- cols[-1]

  siever_with_app(load_example = TRUE, function(app) {
    # The bundled example starts with theoretical assignments. Candidate
    # search requires an explicit change to the empirical workflow; a hidden
    # tab's value alone does not establish that its analysis is available.
    siever_use_empirical_basis(app)
    expect_identical(app$get_value(input = "ingest-basis"), "empirical")
    expect_identical(siever_open_tab(app, "optimize"), "optimize")
    expect_true(app$get_js(paste0(
      "!!document.querySelector('.tab-pane[data-value=optimize].active') && ",
      "document.querySelector('.tab-pane[data-value=optimize]').getClientRects().length > 0"
    )))
    expect_true(app$get_js("document.getElementById('optimize-run').getClientRects().length > 0"))

    # A minimum of two on the nine test example.
    app$set_inputs(`optimize-min_keep` = 2, `optimize-ss` = FALSE)
    siever_wait_render(app)
    expect_match(app$get_text("#optimize-count"), "64 candidate item sets", fixed = TRUE)
    expect_match(app$get_text("#optimize-count"), "Omega cannot be computed for those",
      fixed = TRUE
    )

    # Before any search, the ordering control follows the checkbox: with
    # simple structure off it offers no statistic the search would not
    # compute, and with it on it offers every statistic again.
    offered_now <- function() {
      unlist(app$get_js("Object.keys($('#optimize-order_by')[0].selectize.options)"))
    }
    expect_setequal(offered_now(), c("search", "omega_min", "omega_mean", "cfi", "rmsea", "srmr"))
    app$set_inputs(`optimize-ss` = TRUE)
    siever_wait_render(app)
    expect_setequal(offered_now(), c("search", sieve_optimizer_metrics()$metric))
    app$set_inputs(`optimize-ss` = FALSE)
    siever_wait_render(app)
    expect_setequal(offered_now(), c("search", "omega_min", "omega_mean", "cfi", "rmsea", "srmr"))

    # A maximum to drop far above the pool, typed into the field: the count
    # is immediate and says what the value amounts to, and a search above
    # the cap is refused at once (the work does not grow with the number).
    app$set_inputs(`optimize-max_drop` = 1e6, `optimize-cap` = 5)
    siever_wait_render(app)
    counted <- app$get_text("#optimize-count")
    expect_match(counted, "64 candidate item sets", fixed = TRUE)
    expect_match(counted, "above the cap of 5", fixed = TRUE)
    expect_match(counted, "No construct can lose more than 1 item under these constraints",
      fixed = TRUE
    )
    # Items are excluded, not dropped, in the panel's text.
    expect_match(counted, "a maximum of 1,000,000 allows the same exclusions as 1", fixed = TRUE)
    app$click("optimize-run", timeout_ = 120 * 1000)
    siever_wait_render(app, timeout = 120 * 1000)
    expect_match(app$get_text("#optimize-run_msg"),
      "64 candidate item sets, above the cap of 5",
      fixed = TRUE
    )
    # A value the panel cannot use is refused where it is typed, not replaced.
    app$set_inputs(`optimize-max_drop` = 1, `optimize-cap` = 300, `optimize-min_keep` = 1)
    siever_wait_render(app)
    expect_match(app$get_text("#optimize-count"),
      "must be a whole number, 2 or more, in this panel (you entered 1)",
      fixed = TRUE
    )
    app$set_inputs(`optimize-min_keep` = 2)
    siever_wait_render(app)
    expect_match(app$get_text("#optimize-count"), "64 candidate item sets", fixed = TRUE)

    # The search can take a while on a busy machine, so the click waits for
    # the outputs it changes instead of for a fixed short time.
    app$click("optimize-run", timeout_ = 600 * 1000)
    siever_wait_render(app, timeout = 600 * 1000)
    expect_identical(siever_n_output_errors(app), 0L)

    # Above the table: what happened and why, in words.
    msg <- app$get_text("#optimize-run_msg")
    # The line that says what was searched stands above the table, under
    # its own output (results first).
    expect_match(app$get_text("#optimize-run_head"), "Examined all 64 item sets", fixed = TRUE)
    expect_match(msg, "Omega (min and mean) is unavailable for 63 of 64 item sets", fixed = TRUE)
    expect_match(msg, "expected, not an error", fixed = TRUE)
    expect_match(msg, "Simple structure was not requested for this search", fixed = TRUE)

    # In the table: the order is the researcher's (search order until chosen),
    # the reference is first, and nothing is blank.
    expect_identical(app$get_value(input = "optimize-order_by"), "search")
    rows <- shown_rows(app)
    expect_length(rows, 64L)
    expect_identical(lengths(rows), rep(length(cols), 64L))
    expect_identical(column(rows, at("id")), as.character(1:64))
    expect_identical(column(rows, at("order")), c("reference", rep("search order", 63)))
    cells <- unlist(lapply(rows, function(r) vapply(r, as.character, character(1))))
    expect_false(any(!nzchar(trimws(cells))))
    expect_identical(sum(column(rows, at("omega_min")) == "unavailable"), 63L)
    expect_true(all(grepl("omega unavailable", column(rows, at("status"))[-1], fixed = TRUE)))
    expect_true(all(grepl("simple structure not requested", column(rows, at("status")),
      fixed = TRUE
    )))
    # Eighteen columns are drawn (the position column is hidden), under
    # headers that name each family's model and rows, and none is AIC.
    drawn <- app$get_js(paste0(
      "document.querySelectorAll('#optimize-results .dataTables_scrollBody ",
      "table tbody tr:first-child td').length"
    ))
    expect_identical(as.integer(drawn), 18L)
    head_text <- app$get_text("#optimize-results .dataTables_scrollHead thead")
    # The Optimizer ranks on Robust omega (observed denominator), and the
    # header names it.
    expect_match(head_text,
      "Reliability: Coefficient Omega, \u03c9, as Robust omega (observed denominator), one-factor model of the item codes treated as continuous",
      fixed = TRUE
    )
    expect_match(head_text, "maximum likelihood (ML) with full information ML", fixed = TRUE) # Auto for continuous items
    expect_false(grepl("AIC", head_text, fixed = TRUE))
    # No header offers a sort, so a blank or unusable value cannot be sorted
    # to the top by a click.
    sortable <- app$get_js(paste0(
      "document.querySelectorAll('#optimize-results .dataTables_scrollHead ",
      "th.sorting, #optimize-results .dataTables_scrollHead th.sorting_asc, ",
      "#optimize-results .dataTables_scrollHead th.sorting_desc').length"
    ))
    expect_identical(as.integer(sortable), 0L)

    # The search did not compute simple structure, so the ordering control
    # does not offer complexity or the hyperplane proportion.
    offered <- unlist(app$get_js(
      "Object.keys($('#optimize-order_by')[0].selectize.options)"
    ))
    expect_setequal(offered, c("search", "omega_min", "omega_mean", "cfi", "rmsea", "srmr"))

    # Ordering by min omega: the table stays in search order below the
    # reference, marked not ranked. It does not switch to a fit index.
    app$set_inputs(`optimize-order_by` = "omega_min")
    siever_wait_render(app)
    rows <- shown_rows(app)
    expect_identical(column(rows, at("id")), as.character(1:64))
    expect_identical(
      column(rows, at("order")),
      c(
        sprintf("reference (%s)", column(rows, at("omega_min"))[1]),
        rep("not ranked", 63)
      )
    )

    # Ordering by RMSEA, because the researcher asked: reference first, then
    # the ranked rows in increasing RMSEA, then every row that is not ranked.
    # The order cell carries the value the table is ordered by.
    app$set_inputs(`optimize-order_by` = "rmsea")
    siever_wait_render(app)
    rows <- shown_rows(app)
    label <- column(rows, at("order"))
    rmsea <- column(rows, at("rmsea"))
    expect_identical(label[1], sprintf("reference (%s)", rmsea[1]))
    expect_identical(column(rows, at("id"))[1], "1")
    ranked <- grepl("^[0-9]+ \\(", label)
    not_ranked <- grepl("^not ranked", label)
    expect_gt(sum(ranked), 0L)
    expect_gt(sum(not_ranked), 0L)
    expect_identical(sum(ranked) + sum(not_ranked), 63L)
    expect_true(max(which(ranked)) < min(which(not_ranked)))
    expect_false(is.unsorted(as.numeric(rmsea[ranked])))
    expect_identical(sub("^[0-9]+ \\((.*)\\)$", "\\1", label[ranked]), rmsea[ranked])
    held_back <- rmsea[not_ranked]
    expect_true(all(grepl(" \\*$", held_back) | held_back == "unavailable"))
    expect_match(app$get_text("#optimize-results .dataTables_scrollHead thead"), "order (RMSEA)",
      fixed = TRUE
    )

    # What a reader needs first is on screen without scrolling sideways, at
    # this window and at a 1,440 pixel one: the order with its value, the
    # item set, what it drops, and the status and reason the message above
    # the table points to.
    first <- c("order", "id", "n_items", "dropped", "status")
    frozen <- c("order", "id", "n_items", "dropped")
    fixed_available <- sieve_dt_has_extension("FixedColumns")
    # How many cells of the first row the FixedColumns extension holds in
    # place (its class), and a scroll of the table body that brings a named
    # column's left edge to the right of whatever is frozen.
    fixed_cells <- function(app) {
      as.integer(app$get_js(paste0(
        "document.querySelectorAll('#optimize-results .dataTables_scrollBody table tbody ",
        "tr:first-child td.dtfc-fixed-left').length"
      )))
    }
    scroll_body <- function(app, column = NULL) {
      app$run_js(if (is.null(column)) {
        "document.querySelector('#optimize-results .dataTables_scrollBody').scrollLeft = 0"
      } else {
        sprintf(
          paste0(
            "(function(){var b = document.querySelector('#optimize-results .dataTables_scrollBody');",
            "var td = b.querySelectorAll('table tbody tr:first-child td')[%d]; var fixed = 0;",
            "b.querySelectorAll('table tbody tr:first-child td.dtfc-fixed-left').forEach(",
            "function(t){fixed += t.getBoundingClientRect().width;});",
            "b.scrollLeft = Math.max(0, td.offsetLeft - fixed - 8);})()"
          ),
          match(column, drawn_cols) - 1L
        )
      })
      siever_wait_render(app)
    }
    for (width in c(1600, 1440)) {
      app$set_window_size(width = width, height = 1000)
      siever_wait_render(app)
      scroll_body(app)
      seen <- on_screen(app)
      expect_length(seen, length(drawn_cols))
      expect_true(all(seen[match(first, drawn_cols)]))
      # The statistic the table is ordered by lies to the right. Scrolled to
      # it, the frozen columns keep each row's identity (its order and value,
      # id, items kept, dropped items) in view beside it; without the
      # extension they scroll away and the table says so.
      scroll_body(app, "rmsea")
      seen <- on_screen(app)
      expect_true(seen[match("rmsea", drawn_cols)])
      if (fixed_available) {
        expect_identical(fixed_cells(app), 4L)
        expect_true(all(seen[match(frozen, drawn_cols)]))
      } else {
        expect_identical(fixed_cells(app), 0L)
        expect_false(seen[match("order", drawn_cols)])
        expect_match(app$get_text("#optimize-results"),
          "does not include the DataTables FixedColumns extension",
          fixed = TRUE
        )
      }
      scroll_body(app)
    }
    app$set_window_size(width = 1600, height = 1000)
    siever_wait_render(app)

    # A selected row still indexes the search result: the third row shown is
    # reported to the server by its row in search order, which is its id.
    press_row(app, 3L)
    expect_identical(
      as.character(app$get_value(input = "optimize-results_rows_selected")),
      column(rows, at("id"))[3]
    )
    expect_match(app$get_text("#optimize-detail"),
      sprintf("Item set %s", column(rows, at("id"))[3]),
      fixed = TRUE
    )
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

# The search's Show code and its step of the session script record the
# exported sieve_optimize_items() call, not comments only, with the panel's
# settings and the seed the search used, and evaluating it on the same
# items and ledger reproduces the candidates exactly.
test_that("the Optimizer records the runnable call it made, and the recorded call reproduces the candidates", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  skip_if_not_installed("psych")
  st <- optimizer_state()
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(max_drop = 1, min_keep = 2, ss = TRUE, cap = 300, undersized = "hold", must_keep = "t1_visual_perception")
    session$setInputs(run = 1)
    r <- res()
    expect_null(r$error)
    code <- r$code_view$code
    call_at <- grep("optimizer_search <- SIEVER::sieve_optimize_items(items, meta,", code, fixed = TRUE)
    expect_length(call_at, 1L)
    recorded <- paste(code[call_at:(call_at + 2L)], collapse = "\n")
    expect_match(recorded, "max_drop = 1, min_keep = 2, cap = 300, simple_structure = TRUE", fixed = TRUE)
    expect_match(recorded, sprintf("must_keep = \"t1_visual_perception\", undersized = \"hold\", seed = %dL)", r$rng$seed), fixed = TRUE)
    expect_true("optimizer_search$table" %in% code)
    env <- new.env(parent = asNamespace("SIEVER"))
    env$items <- shiny::isolate(state$items)
    env$meta <- shiny::isolate(state$meta)
    eval(parse(text = recorded), env)
    expect_identical(env$optimizer_search$table, r$table)
    expect_identical(env$optimizer_search$rng$seed, r$rng$seed)
  })
  # Without simple structure no seed is used, and none is recorded.
  code <- sieve_optimizer_search_code(list(max_drop = 0, min_keep = 3, cap = 300, undersized = "hold"),
    simple_structure = FALSE, seed = NULL)
  expect_match(paste(code, collapse = "\n"), "simple_structure = FALSE,\n  min_facet = NULL, must_keep = NULL, undersized = \"hold\", seed = NULL)", fixed = TRUE)
})

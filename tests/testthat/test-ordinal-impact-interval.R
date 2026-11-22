# Ordinal expected-score impact carries pointwise intervals from draws of
# the item parameters: sieve_ordinal_fit_impact() never returns point values
# alone with "No sampling interval is supplied."

ordinal_impact_fixture <- function() {
  d <- withr::with_seed(419, {
    eta <- stats::rnorm(1000) + rep(c(0, .5), each = 500)
    x <- as.data.frame(sapply(seq_len(5), function(j) {
      as.integer(cut(
        .8 * eta + stats::rnorm(1000, sd = .6) + if (j == 5) rep(c(0, .5), each = 500) else 0,
        c(-Inf, -.7, 0, .7, Inf)
      ))
    }))
    names(x) <- paste0("y", seq_len(5))
    x$g <- rep(c("A", "B"), each = 500)
    x
  })
  m <- sieve_build_meta(d, paste0("y", seq_len(5)), list(Scale = paste0("y", seq_len(5))))
  m$type <- "ordered"
  r <- sieve_anchor_sensitivity(d, "g", m, list(Four = paste0("y", 1:4), Three = paste0("y", 1:3)),
    rationale = "Known fixture anchors", discovery = FALSE
  )
  list(fit = r$models$Four$fit, scores = stats::setNames(rep(list(1:4), 5L), paste0("y", 1:5)))
}

test_that("the group matrices at other parameter values are lavaan's own", {
  skip_on_cran()
  x <- ordinal_impact_fixture()
  fit <- x$fit
  est <- lavaan::lavInspect(fit, "est")
  # At the estimate, the matrices are those lavaan reports.
  at <- sieve_lavaan_group_matrices(fit, lavaan::coef(fit), est)
  for (g in 1:2) {
    for (nm in c("lambda", "nu", "tau", "theta")) {
      expect_identical(dimnames(at[[g]][[nm]]), dimnames(est[[g]][[nm]]))
      expect_equal(unclass(at[[g]][[nm]]), unclass(est[[g]][[nm]]), tolerance = 1e-12, ignore_attr = TRUE)
    }
  }
  # Moving one free parameter (y5's first threshold in the focal group,
  # free there because y5 is not an anchor) moves that element alone.
  pt <- lavaan::parTable(fit)
  i <- pt$free[pt$lhs == "y5" & pt$op == "|" & pt$rhs == "t1" & pt$group == 2L]
  expect_true(i > 0L)
  moved <- lavaan::coef(fit)
  moved[i] <- moved[i] + 0.1
  m <- sieve_lavaan_group_matrices(fit, moved, est)
  expect_equal(m[[2]]$tau["y5|t1", 1] - est[[2]]$tau["y5|t1", 1], 0.1, tolerance = 1e-12)
  expect_equal(m[[1]]$tau, at[[1]]$tau, tolerance = 0)
  expect_equal(m[[2]]$tau[-which(rownames(m[[2]]$tau) == "y5|t1"), 1], at[[2]]$tau[-which(rownames(at[[2]]$tau) == "y5|t1"), 1], tolerance = 0)
})

test_that("sieve_ordinal_fit_impact() reports pointwise intervals from parameter draws, reproducibly", {
  skip_on_cran()
  x <- ordinal_impact_fixture()
  run <- function(...) {
    sieve_ordinal_fit_impact(x$fit, x$scores, "A", "B", anchors = "Fixture anchor declaration", ...)
  }
  a <- run(B = 200L, seed = 1)
  limits <- c("signed_lower", "signed_upper", "absolute_lower", "absolute_upper", "rms_lower", "rms_upper")
  # Limit columns and an interval record that counts every draw.
  expect_true(all(limits %in% names(a$table)))
  expect_true(all(limits %in% names(a$totals)))
  expect_identical(a$interval$status, "completed")
  expect_identical(a$interval$attempted, 200L)
  expect_identical(a$interval$used + a$interval$failed, 200L)
  expect_identical(a$interval$rng$seed, 1L)
  expect_true(a$interval$rng$supplied)
  expect_identical(a$interval$conf_level, 0.95)
  expect_true(all(is.finite(as.matrix(a$table[, limits]))))
  expect_true(all(a$table$signed_lower <= a$table$signed_upper))
  # Absolute and RMS impact are nonnegative, and so are their limits.
  expect_true(all(a$table$absolute_lower >= 0 & a$table$rms_lower >= 0))
  # The released item's signed impact, well away from zero, lies inside its
  # interval.
  y5 <- a$table[a$table$item == "y5", ]
  expect_gt(abs(y5$signed), .05)
  expect_true(y5$signed_lower < y5$signed && y5$signed < y5$signed_upper)
  expect_match(a$limitation, "95% pointwise intervals from", fixed = TRUE)
  expect_match(a$limitation, "biased upward near 0", fixed = TRUE)
  expect_false(grepl("No sampling interval", a$limitation, fixed = TRUE))
  # Each factor's impact object is the same impact, with the same limits
  # and limitation (constructs$Scale has the limit columns, and not "No
  # sampling interval is supplied").
  scale <- a$constructs$Scale
  expect_identical(scale$limitation, a$limitation)
  expect_false(grepl("No sampling interval", scale$limitation, fixed = TRUE))
  expect_identical(scale$items[, c("item", limits)], a$table[a$table$factor == "Scale", c("item", limits)], ignore_attr = TRUE)
  expect_identical(unlist(scale$total[, limits]), unlist(a$totals[a$totals$factor == "Scale", limits]))
  # The point estimates do not depend on the draws.
  point <- run(B = 0L)
  expect_identical(point$table[, c("item", "weight", "signed", "absolute", "rms", "factor")],
    a$table[, c("item", "weight", "signed", "absolute", "rms", "factor")])
  expect_identical(point$interval$status, "not requested")
  expect_true(all(is.na(as.matrix(point$table[, limits]))))
  expect_identical(point$constructs$Scale$limitation, point$limitation)
  expect_match(point$constructs$Scale$limitation, "No interval: B = 0 requests point impact alone.", fixed = TRUE)
  # The same seed gives the same limits; the same draws at 0.90 give limits
  # inside the 0.95 ones.
  expect_identical(run(B = 200L, seed = 1)$table, a$table)
  b <- run(B = 200L, seed = 1, conf_level = .90)
  expect_true(all(b$table$signed_lower >= a$table$signed_lower & b$table$signed_upper <= a$table$signed_upper))
  expect_identical(b$interval$conf_level, 0.90)
  # Too few draws for the level, or a bad level, is refused.
  expect_error(run(B = 10L), "at least 39 parameter draws for a 95% interval", fixed = TRUE)
  expect_error(run(conf_level = 95), "`conf_level` must be one number strictly between 0 and 1; received 95.", fixed = TRUE)
})

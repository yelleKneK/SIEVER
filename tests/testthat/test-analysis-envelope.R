# The siever_analysis envelope and its interval descriptor, and the schema
# acceptance check. The envelope carries a schema version and one interval
# descriptor that confint(), summary() and print() read; the acceptance
# test walks every service of sieve_analyze() and checks both.

envelope_fields <- c(
  "schema_version", "analysis", "service", "arguments_hash", "design", "versions",
  "engine_identity", "result", "status", "diagnostics", "intervals"
)

descriptor_columns <- c(
  table = "character", term = "character", estimate = "numeric", se = "numeric",
  lower = "numeric", upper = "numeric", conf_level = "numeric", side = "character",
  method = "character", kind = "character", scale = "character", df = "numeric",
  transformation = "character", n_used = "integer", B_used = "integer",
  recomputed = "logical", reason = "character", rerun = "character"
)

envelope_hs9 <- function() lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]

envelope_meta <- function(items) {
  data.frame(
    item = names(items), label = names(items),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
}

# The checks every analysis must pass, whatever its service: the envelope's
# fields and version, the descriptor's columns and types, a row either with
# its limits at a level in (0, 1) or with the reason it has none, and the
# three generics reading the descriptor.
check_envelope <- function(a, label) {
  expect_s3_class(a, "siever_analysis")
  expect_identical(names(a), envelope_fields, label = label)
  expect_identical(a$schema_version, 1L, label = label)
  expect_true(is.character(a$status$code) && length(a$status$code) == 1L, label = label)
  iv <- a$intervals
  expect_true(is.data.frame(iv) && nrow(iv) > 0L, label = label)
  expect_identical(names(iv), names(descriptor_columns), label = label)
  types <- vapply(iv, function(x) if (is.integer(x)) "integer" else if (is.double(x)) "numeric" else class(x)[1L], character(1))
  expect_identical(types, descriptor_columns, label = label)
  expect_false(any(iv$recomputed), label = label)
  with_limits <- !is.na(iv$term) & is.na(iv$reason)
  # A row without limits says why.
  expect_true(all(nzchar(iv$reason[!with_limits]) & !is.na(iv$reason[!with_limits])), label = label)
  # A row with limits has a level in (0, 1), a side, a method, a kind, and
  # its limits in order (the one limit of a one-sided bound).
  r <- iv[with_limits, , drop = FALSE]
  expect_true(all(r$conf_level > 0 & r$conf_level < 1), label = label)
  expect_true(all(r$side %in% c("two-sided", "upper", "lower")), label = label)
  expect_true(all(r$kind %in% c(
    "wald", "bootstrap", "lavaan_bootstrap", "profile", "order_statistic", "exact_binomial",
    "noncentral", "wald_without_se", "one_sided", "other"
  )), label = label)
  expect_true(all(nzchar(r$method)), label = label)
  two <- r$side == "two-sided"
  expect_true(all(is.finite(r$lower[two]) & is.finite(r$upper[two]) & r$lower[two] <= r$upper[two]), label = label)
  expect_true(all(is.finite(r$upper[r$side == "upper"])), label = label)
  # An empty table name means the analysis has no table of limits, and then
  # it is the only row.
  if (anyNA(iv$table)) expect_identical(nrow(iv), 1L, label = label)
  # summary() and print() read the descriptor.
  s <- summary(a)
  expect_identical(s$schema_version, 1L, label = label)
  expect_identical(s$intervals, sieve_intervals_overview(iv), label = label)
  printed <- utils::capture.output(print(a))
  expect_true(any(startsWith(printed, "Intervals: ")), label = label)
  expect_true(any(printed == "Schema version: 1 "), label = label)
  # confint() with the stored level returns the descriptor's rows of the
  # main table, exactly, or refuses with the descriptor's own reason.
  main <- iv$table[!is.na(iv$table)][1L]
  rows <- iv[iv$table %in% main & !is.na(iv$term) & is.na(iv$reason), , drop = FALSE]
  limits <- tryCatch(stats::confint(a), error = function(e) e)
  if (is.na(main) || !nrow(rows)) {
    expect_s3_class(limits, "error")
    if (is.na(main)) expect_identical(conditionMessage(limits), iv$reason[1L], label = label)
  } else if (any(rows$side != "two-sided")) {
    expect_s3_class(limits, "error")
    expect_identical(conditionMessage(limits), unique(rows$rerun[rows$side != "two-sided"])[1L], label = label)
  } else {
    expect_false(inherits(limits, "error"), label = label)
    expect_identical(rownames(limits), rows$term, label = label)
    expect_identical(unname(limits[, "lower"]), rows$lower, label = label)
    expect_identical(unname(limits[, "upper"]), rows$upper, label = label)
    expect_identical(attr(limits, "table"), main, label = label)
    expect_identical(attr(limits, "recomputed"), FALSE, label = label)
  }
  invisible(iv)
}

test_that("the envelope carries its schema version and an interval descriptor that the generics read", {
  # The object carries its schema_version and an interval descriptor;
  # confint() returns the descriptor's limits rather than rebuilding them
  # from the result, and print() states them.
  ratings <- withr::with_seed(1, matrix(sample(1:4, 40, TRUE), 10, 4))
  content <- sieve_analyze("content", ratings = ratings)
  iv <- check_envelope(content, "content")
  expect_identical(iv$term, content$result$item)
  expect_identical(iv$n_used, as.integer(content$result$n_experts))
  expect_identical(unique(iv$conf_level), DMAR::dmar_metadata(content$result)$conf_level)
  printed <- utils::capture.output(print(content))
  expect_true("Intervals: Results (10 two-sided at 0.95)" %in% printed)
  expect_identical(attr(as.data.frame(content), "schema_version"), 1L)
  # confint() reads the descriptor the object carries, not the result: an
  # edited descriptor is what it returns.
  edited <- content
  edited$intervals$upper[1L] <- 0.999
  expect_identical(unname(stats::confint(edited)[1L, "upper"]), 0.999)
  expect_false(identical(unname(stats::confint(content)[1L, "upper"]), 0.999))
  # An analysis without limits records the reason once.
  screening <- sieve_analyze("screening", data = envelope_hs9())
  iv <- check_envelope(screening, "screening")
  expect_identical(nrow(iv), 1L)
  expect_true(is.na(iv$table) && is.na(iv$term))
  expect_match(iv$reason, "holds no confidence limits", fixed = TRUE)
  # A failed analysis records why it has no estimates.
  failed <- sieve_analyze("content", ratings = matrix(c(1, 2, 5, 7), 2L))
  iv <- check_envelope(failed, "failed content")
  expect_identical(iv$reason, paste("This analysis has no estimates.", failed$status$message))
})

test_that("each parameter of a multiple group fit records the cases of its own group", {
  hs <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3", hs, group = "school", std.lv = TRUE)
  obj <- structure(list(analysis = "cfa", result = fit, status = list(code = "ok", message = "")), class = "siever_analysis")
  iv <- sieve_intervals_of(obj)
  nobs <- lavaan::lavInspect(fit, "nobs")
  second <- endsWith(iv$term, ".g2")
  expect_true(any(second))
  expect_identical(unique(iv$n_used[second]), as.integer(nobs[2L]))
  expect_identical(unique(iv$n_used[!second]), as.integer(nobs[1L]))
})

test_that("every sieve_analyze() service returns the versioned envelope and a complete interval descriptor", {
  # The schema acceptance check: one walk over the sixteen services,
  # each with a small fixture. Not on the CRAN check's clock (it fits every
  # model family); the full suite runs it.
  skip_on_cran()
  items <- envelope_hs9()
  meta <- envelope_meta(items)
  hs <- lavaan::HolzingerSwineford1939
  fit1 <- lavaan::cfa("Visual =~ x1 + x2 + x3", items, std.lv = TRUE)
  fit2 <- lavaan::cfa("Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6", items, std.lv = TRUE)
  ordered <- as.data.frame(lapply(items[, 1:6], function(x) as.integer(cut(x, 4))))
  ratings <- withr::with_seed(1, matrix(sample(1:4, 40, TRUE), 10, 4))
  map <- data.frame(
    construct = "Democracy", wave = rep(c("1960", "1965"), each = 4L),
    item = rep(c("press", "opposition", "elections", "legislature"), 2L),
    variable = paste0("y", 1:8), wave_order = rep(1:2, each = 4L)
  )
  rel_spec <- sieve_reliability_spec(paste0("x", 4:6), rep("continuous", 3),
    construct = "Textual", missing = "fiml", ci_method = "mlr", denominator = "model_implied"
  )
  calls <- list(
    cfa = function() sieve_analyze("cfa", items_df = items, meta = meta),
    efa = function() sieve_analyze("efa", data = items, estimator = "ML", ks = 1:2),
    reliability = function() sieve_analyze("reliability", data = items[, 4:6], spec = rel_spec),
    retention = function() {
      suppressWarnings(sieve_analyze("retention",
        X = items[, 1:6], types = rep("continuous", 6),
        n_iter = 200L, max_k = 2L, seed = 1
      ))
    },
    screening = function() sieve_analyze("screening", data = items),
    sem = function() sieve_analyze("sem", items_df = items, raw = items, meta = meta, outcomes = "Speed", predictors = c("Visual", "Textual")),
    bifactor = function() suppressWarnings(sieve_analyze("bifactor", items_df = items, meta = meta)),
    invariance = function() {
      sieve_analyze("invariance",
        data = hs[, c(paste0("x", 1:9), "school")], group = "school",
        meta = meta, levels = c("configural", "metric")
      )
    },
    cross_validation = function() sieve_analyze("cross_validation", data = items, meta = meta, seed = 1),
    longitudinal = function() {
      sieve_analyze("longitudinal",
        data = lavaan::PoliticalDemocracy, mapping = map, level = "metric",
        justification = "The same four indicators were rated at both waves"
      )
    },
    factor_scores = function() sieve_analyze("factor_scores", fit = fit1, data = items[, 1:3], conf_level = .90),
    norms = function() sieve_analyze("norms", data = items, meta = meta),
    irt = function() suppressWarnings(sieve_analyze("irt", data = ordered)),
    htmt = function() sieve_analyze("htmt", data = items, blocks = list(V = paste0("x", 1:3), T = paste0("x", 4:6)), B = 200, seed = 1),
    ave = function() sieve_analyze("ave", fit = fit2),
    content = function() sieve_analyze("content", ratings = ratings)
  )
  # Every service of the dispatcher is walked.
  expect_setequal(names(calls), names(sieve_analysis_services()))
  # The main interval table of each service, NA where it has none.
  main <- c(
    cfa = "parameters", efa = "Results", reliability = "reliability", retention = "Results: fit: table",
    screening = NA, sem = "parameters", bifactor = "parameters", invariance = "Results: compare: table",
    cross_validation = "table", longitudinal = "parameters", factor_scores = "uncertainty",
    norms = "Results: uncertainty", irt = NA, htmt = "Results", ave = "Results", content = "Results"
  )
  for (analysis in names(calls)) {
    a <- calls[[analysis]]()
    expect_identical(a$analysis, analysis)
    iv <- check_envelope(a, analysis)
    expect_identical(iv$table[1L], unname(main[[analysis]]), label = analysis)
    has <- !is.na(iv$term) & is.na(iv$reason)
    if (analysis %in% c("screening", "irt")) {
      expect_false(any(has), label = analysis)
    } else if (identical(analysis, "ave")) {
      # AVE without a bootstrap: each factor is listed, saying it has no
      # interval and how to obtain one.
      expect_identical(iv$term, a$result$factor)
      expect_true(all(grepl("estimated without an interval", iv$reason, fixed = TRUE)))
      expect_identical(unique(iv$n_used), as.integer(lavaan::lavInspect(fit2, "ntotal")))
    } else {
      expect_true(any(has), label = analysis)
      # Every analysis with limits records the cases they rest on, except a
      # respondent's factor score interval, which rests on no sample.
      if (identical(analysis, "factor_scores")) {
        expect_true(all(is.na(iv$n_used)))
      } else {
        expect_true(all(is.finite(iv$n_used[has]) & iv$n_used[has] > 0L), label = analysis)
      }
    }
    if (identical(analysis, "htmt")) {
      expect_identical(iv$side, "upper")
      expect_identical(iv$kind, "one_sided")
      expect_true(is.na(iv$lower))
      expect_identical(iv$upper, a$result$ci_upper)
      expect_identical(iv$n_used, nrow(items))
      expect_identical(iv$B_used, 200L)
    }
    if (identical(analysis, "cross_validation")) {
      rmsea <- iv$table == "table" & endsWith(iv$term, ": RMSEA")
      expect_identical(iv$n_used[rmsea], as.integer(a$result$table$n))
      expect_identical(unique(iv$conf_level[rmsea]), a$result$rmsea_conf_level)
    }
  }
})

test_that("AVE and HTMT without a bootstrap name the level asked for and the replications DMAR needs there", {
  # An HTMT run without a bootstrap records its rerun at the level asked for
  # (never "level NA" or "..., NA at NA)") and advises the replications DMAR
  # needs at that level (B = 200 is DMAR's one-sided minimum at 0.95 only;
  # at 0.99 DMAR asks for "at least 1000"); AVE's reason names its level and
  # its rerun's <min_B> is documented.
  items <- envelope_hs9()
  blocks <- list(V = paste0("x", 1:3), T = paste0("x", 4:6))
  no_na <- function(x) expect_false(any(grepl("\\bNA\\b", x)), label = paste(x, collapse = " | "))
  h99 <- sieve_analyze("htmt", data = items, blocks = blocks, conf_level = .99)
  iv <- h99$intervals
  expect_true(is.na(iv$upper) && is.na(iv$conf_level))
  expect_identical(iv$reason, paste0(
    "HTMT was estimated without a bound; run sieve_analyze(\"htmt\", ..., conf_level = 0.99, B = 1000 or more, ",
    "seed = ...) for a one-sided upper bound at that level"
  ))
  no_na(iv$reason)
  no_na(iv$rerun)
  expect_match(iv$rerun, "conf_level = <level>, B = <min_B> or more", fixed = TRUE)
  # Filled in, the rerun names DMAR's one-sided minimum at each level.
  expect_match(sieve_confint_fill_rerun(iv$rerun, .99, one_sided = TRUE), "conf_level = 0.99, B = 1000 or more", fixed = TRUE)
  expect_match(sieve_confint_fill_rerun(iv$rerun, .90, one_sided = TRUE), "conf_level = 0.9, B = 100 or more", fixed = TRUE)
  # The call named no level: DMAR's default, 0.95, and its 200.
  h95 <- sieve_analyze("htmt", data = items, blocks = blocks)
  expect_match(h95$intervals$reason, "conf_level = 0.95, B = 200 or more", fixed = TRUE)
  # Following the advice at 0.90 forms the bound rather than being refused.
  h90 <- sieve_analyze("htmt", data = items, blocks = blocks, conf_level = .90)
  expect_match(h90$intervals$reason, "conf_level = 0.9, B = 100 or more", fixed = TRUE)
  bound <- sieve_analyze("htmt", data = items, blocks = blocks, conf_level = .90, B = 100, seed = 1)
  expect_identical(bound$status$code, "ok")
  expect_true(is.finite(bound$intervals$upper) && is.na(bound$intervals$reason))
  expect_identical(bound$intervals$conf_level, 0.9)
  no_na(bound$intervals$rerun)
  # AVE without a bootstrap names the level and DMAR's two-sided minimum,
  # 10 / ((1 - C) / 2).
  fit <- lavaan::cfa("Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6", items, std.lv = TRUE)
  a99 <- sieve_analyze("ave", fit = fit, conf_level = .99)
  expect_identical(unique(a99$intervals$reason), paste0(
    "AVE was estimated without an interval; run sieve_analyze(\"ave\", fit = ..., ci_method = \"percentile\", ",
    "conf_level = 0.99, B = 2000 or more, seed = ...) for a percentile interval at that level"
  ))
  expect_match(sieve_analyze("ave", fit = fit)$intervals$reason[1L], "conf_level = 0.95, B = 400 or more", fixed = TRUE)
  expect_match(sieve_confint_fill_rerun(a99$intervals$rerun[1L], .90), "conf_level = 0.9, B = 200 or more", fixed = TRUE)
  # The help documents both placeholders of the rerun column.
  root <- compat_source_root()
  rd <- if (!is.null(root)) {
    tools::parse_Rd(file.path(root, "man", "sieve_analyze.Rd"))
  } else {
    tools::Rd_db("SIEVER")[["sieve_analyze.Rd"]]
  }
  rd <- paste(utils::capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE))), collapse = " ")
  expect_match(rd, "<min_B>", fixed = TRUE)
  expect_match(rd, "<level>", fixed = TRUE)
})

test_that("the replications advised for a bootstrap limit are never fewer than DMAR's 100", {
  # DMAR 1.1.0 refuses fewer than 100 replications, so the advice at 0.80
  # and 0.60 is 100, never the 50 or 25 the levels alone would suggest.
  expect_identical(sieve_confint_dmar_min_B(.80, one_sided = TRUE), 100L)
  expect_identical(sieve_confint_dmar_min_B(.60, one_sided = TRUE), 100L)
  expect_identical(sieve_confint_dmar_min_B(.60, one_sided = FALSE), 100L)
  expect_identical(sieve_confint_dmar_min_B(.95, one_sided = TRUE), 200L)
  expect_identical(sieve_confint_dmar_min_B(.95, one_sided = FALSE), 400L)
  expect_identical(sieve_confint_dmar_min_B(.99, one_sided = TRUE), 1000L)
})

test_that("a share of unusable draws equal to the tail area raises the caution", {
  # 25 of 1,000 at 0.95 is exactly the tail area; (1 - 0.95) / 2 is a hair
  # above 0.025 in floating point, and the comparison of shares missed it.
  expect_true(sieve_bootstrap_caution_reached(25, 1000, .95))
  expect_false(sieve_bootstrap_caution_reached(24, 1000, .95))
  expect_false(sieve_bootstrap_caution_reached(0, 1000, .95))
  expect_true(sieve_bootstrap_caution_reached(50, 1000, .90))
})

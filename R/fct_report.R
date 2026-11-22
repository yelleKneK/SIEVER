# fct_report.R
# The manuscript draft builder: assembles a methods-and-results passage from
# the current state of the analysis, with every number computed from the
# live data when it is assembled or taken from a record of the same
# computation on the same data, which the draft names (never transcribed
# from a display). The draft is a starting
# point in the author's own voice to edit, not a finished section; square
# bracketed placeholders mark what only the researcher can supply. This is
# the measurement-transparency companion to the reproducible code log: the
# log discloses the computations, the draft discloses the decisions in
# prose (Flake & Fried, 2020).
#
# What the draft computes is its own: it refits the correlated-factors CFA
# with the study's estimator on the item data and the ledger as they stand
# when it is assembled, and it computes the reliability coefficient the
# Reliability tab reports by default (Robust omega (observed denominator)
# for continuous items, and for ordered or binary items with the
# denominator built from the unrestricted polychoric correlations, with
# Omega (model-implied denominator) beside it), on the items as the tab's
# item treatment takes
# them and with the study estimator's treatment of incomplete cases,
# through the Reliability tab's own functions. The draft does not read the
# results shown on the CFA or other tabs, with two exceptions, each taken
# from a record of the same computation on the same data and named where it
# appears: a bootstrap interval the Reliability tab ran for the same
# request, and the unbiased SRMR, or DMAR's refusal of it, the CFA tab
# computed for the identical fit (the same model, data, respondents and
# engines). It does not reproduce an analysis run earlier in the session
# with other settings or another item pool. It says so, and names the data
# and ledger revision it used, so a reader can tell which model and which
# data the numbers describe.

#' Assemble a Methods and Results Draft From the Current Analysis
#'
#' Computes, from the active items: sample size and missingness (with the
#' respondents removed on the Screening tab, counted with their reasons),
#' the measurement design (constructs and their item counts), the
#' estimator implied by the confirmed item types and its treatment of
#' incomplete cases, the correlated-factors CFA with its fit indices (RMSEA
#' with its interval at the RMSEA level, 90 percent unless
#' `options(sieve.rmsea_conf_level = )` sets another, whatever the
#' reliability confidence level; CFI, TLI, SRMR with the unbiased SRMR of
#' Maydeu-Olivares (2017) and its interval at the RMSEA level where DMAR
#' computes it, whatever its estimated time, unless `unbiased_srmr =
#' FALSE`), and per-construct
#' reliability: the coefficient the Reliability tab reports by default for
#' each construct (robust omega for continuous items, with model implied
#' omega and its MLR (sandwich) SE interval beside it; categorical omega for
#' ordered or binary items), on the items as the tab's item treatment takes
#' them, with the study estimator's treatment of incomplete cases, its case
#' treatment and n, and its interval at the Reliability tab's confidence
#' level, 95 percent by default (a bootstrap interval the Reliability
#' tab ran on the same data, or a statement that the bootstrap has not been
#' run). Returns markdown text. Sections that cannot
#' be computed (an unfit model, a too-small construct) state that plainly
#' rather than being silently omitted.
#'
#' The draft states its provenance: the numbers are computed when it is
#' assembled, from `items` and `meta` as supplied (identified by their
#' content keys, `sieve_data_key()` and `sieve_meta_key()`), by refitting
#' the correlated-factors CFA and computing reliability with the settings
#' given. It does not read saved results, with two exceptions, each taken
#' from a record in `analyses` of the same computation on the same data
#' and named where it appears: a bootstrap interval that matches the
#' reliability request and the data exactly, and the unbiased SRMR, or
#' DMAR's refusal of it, of a CFA record of the identical fit (see
#' `unbiased_srmr`). It does not reproduce earlier analyses of the session;
#' the statement and the model syntax it refit are written into the draft.
#'
#' The CFA goes through the same memory refusal as the CFA page
#' (`sieve_spec_mlr_guard()`): a fit the page would refuse is not started,
#' and the draft reports the refusal in place of fit indices. The reference
#' list at the end is built from [sieve_method_references()] for the
#' methods the draft computed with (the estimator family, the missing data
#' treatment, the fit indices when the model was fit, each reliability
#' coefficient and the interval method actually used) and the software it
#' ran in, with the installed versions; a method the draft did not use is
#' not cited.
#'
#' # Interval Levels
#'
#' The draft's intervals are at two levels, and every interval names its
#' level where it appears. The omega intervals (the bootstrap interval of
#' the coefficient the Reliability tab reports by default, and the interval
#' of model implied omega beside it) are at the Reliability tab's confidence
#' level, `reliability$conf_level`, or 0.95 when `reliability` is `NULL`.
#' Every RMSEA interval, that of the correlated-factors CFA and that of each
#' construct's one-factor model, is at the RMSEA level,
#' [sieve_default_rmsea_conf_level()]: `getOption("sieve.rmsea_conf_level")`
#' when it is one number strictly between 0 and 1, else 0.90, the level
#' whose limits decide the one-sided tests of close fit and of not close fit
#' at .05 (MacCallum, Browne & Sugawara, 1996). The Reliability tab's level
#' does not reach an RMSEA interval, so a draft with the reliability
#' confidence level at 0.80 reports 80 percent omega intervals beside 90
#' percent RMSEA intervals, and `options(sieve.rmsea_conf_level = 0.95)` puts
#' the RMSEA intervals at 95 percent and leaves the omega intervals where
#' they are.
#'
#' @param items The item data (`state$items`).
#' @param meta The item ledger (`state$meta`).
#' @param dataset_name A label for the data source.
#' @param revision The inputs the draft is computed from, as
#'   `sieve_revision()` records them (`dataset_id`, `data_key`,
#'   `meta_key`). `NULL` computes the keys from `items` and `meta`.
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   with FIML when every item is continuous, WLSMV when any is ordered or
#'   binary), `"mlr"` (MLR with FIML), `"ml_fiml"` (ML with FIML), `"ml"`
#'   (ML on the complete cases) or `"wlsmv"` (WLSMV, every item treated as
#'   ordered): the study's estimator setting (the Ingest tab), which the draft's
#'   CFA is fitted under and its method sentence states.
#' @param sampling_design Declared sampling unit and optional cluster column.
#'   This refitting draft supports independent rows; dependent designs must
#'   be reported from saved analyses using a supported design-correct method.
#' @param reliability The Reliability tab's settings
#'   (`state$reliability_settings`), or `NULL`: the draft follows its item
#'   treatment (the `"study"` choice resolves from `estimator`), its
#'   reporting scope and its confidence level (the level of the omega
#'   intervals; it does not reach any RMSEA interval, whose level is
#'   [sieve_default_rmsea_conf_level()]), and reports the tab's
#'   default coefficient (see `sieve_draft_reliability()`).
#' @param removed The respondents removed from the analysis, as
#'   `sieve_removed_table()` returns them (`row` and `reason` are read), or
#'   `NULL` when none were. Identifiers are never written into the draft.
#' @param analyses The analysis registry (`state$analyses`), searched for a
#'   completed bootstrap interval of the same reliability request on the
#'   same data and for an unbiased SRMR computed on the CFA tab for the
#'   identical fit (see `unbiased_srmr`), or `NULL`.
#' @param scores The operational score specification in force, one entry
#'   per construct as `sieve_score_specification()` returns it (items,
#'   method, minimum answered count or proportion), or `NULL`: the method
#'   paragraph states how each construct is scored and how many respondents
#'   meet its rule, the specification the Validity page, the norms and the
#'   scoring syntax read.
#' @param unbiased_srmr `TRUE` (the default) to report the unbiased SRMR of
#'   Maydeu-Olivares (2017) with its interval at the RMSEA level
#'   (`DMAR::sem_residual_fit()`) beside the SRMR, computed whatever its
#'   estimated time: the draft is an explicit request, not a page that
#'   follows the run policy (`options(sieve.auto_run = )`). Its time grows
#'   with the cube of the number of residual covariances (about 4 seconds
#'   at 40 items and 4.5 minutes at 80, as measured), so `FALSE`
#'   leaves it out of a draft of a large model; the draft then says so.
#'   When `analyses` holds a CFA record of the identical fit (the same
#'   model, item data, ledger, respondents, estimator, missing data
#'   treatment, identification and lavaan and DMAR installations) whose
#'   unbiased SRMR was computed at the same level, that value, or DMAR's
#'   refusal, is taken from the record and the draft names it, since
#'   computing it again would give the same result. Where DMAR does not
#'   compute it (incomplete data under FIML, ordered items) the draft gives
#'   DMAR's reason, and it says the value was not computed only when
#'   `unbiased_srmr = FALSE` left it out. In the app the draft's button
#'   states the estimated time when it exceeds the run policy's two
#'   seconds.
#' @return A single character string of markdown.
#' @author Ken Kelley
#' @examples
#' # The draft fits the measurement model and its reliability first.
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' draft <- sieve_methods_draft(items, meta, dataset_name = "Holzinger and Swineford (1939)")
#' cat(substr(draft, 1, 600))
#' @export
sieve_methods_draft <- function(items, meta, dataset_name = "the data", revision = NULL,
                                estimator = "auto", sampling_design = list(unit = "independent"),
                                reliability = NULL, removed = NULL, analyses = NULL, scores = NULL,
                                unbiased_srmr = TRUE) {
  problem <- sieve_sampling_problem(list(sampling_design = sampling_design), "independent")
  if (!is.null(problem)) stop(
    paste(
      problem,
      "Report supported saved analyses instead of this independent-row refitting draft."
    ),
    call. = FALSE
  )
  if (is.null(revision)) {
    revision <- list(
      dataset_id = NA_character_, data_key = sieve_data_key(items),
      meta_key = sieve_meta_key(meta)
    )
  }
  active <- meta[meta$status == "active", , drop = FALSE]
  cons <- unique(active$construct[sieve_construct_assigned(active$construct)])
  its <- intersect(active$item, names(items))
  X <- items[, its, drop = FALSE]
  X[] <- lapply(X, function(v) suppressWarnings(as.numeric(v)))
  n_total <- nrow(X)
  n_complete <- sum(stats::complete.cases(X))
  pct_missing <- if (length(its)) 100 * mean(is.na(as.matrix(X))) else NA_real_
  n_removed <- if (is.data.frame(removed)) nrow(removed) else 0L

  spec <- sieve_lavaan_spec(meta, estimator = estimator)
  est_txt <- sieve_draft_estimator_text(spec$estimator, spec$missing)

  con_txt <- vapply(cons, function(cn) {
    k <- sum(active$construct == cn)
    sprintf("%s (%d items)", cn, k)
  }, character(1))

  sample_txt <- if (n_removed) {
    sprintf(
      paste0(
        "Of the %d respondents in the data as loaded, %d %s removed from the analysis during ",
        "screening (%s), so the analytic sample comprised %d respondents on %d items "
      ),
      n_total + n_removed, n_removed, if (n_removed == 1L) "was" else "were",
      sieve_draft_reasons(removed), n_total, length(its)
    )
  } else {
    sprintf("The analytic sample comprised %d respondents on %d items ", n_total, length(its))
  }

  # Reliability: the Reliability tab's default coefficient, one request per
  # construct resolved by the tab's own functions, with the study
  # estimator's treatment of incomplete cases.
  rel <- sieve_draft_reliability(items, meta, reliability, estimator, revision, analyses,
    missing = spec$missing
  )

  lines <- c(
    "## Method (draft; edit before use)",
    "",
    sprintf(
      paste0(
        "Participants were [describe the sample and its recruitment]. %s",
        "(%d complete on every item; %.1f%% of item responses missing), from %s. ",
        "The instrument was designed to measure %s: %s."
      ),
      sample_txt, n_complete, pct_missing, dataset_name,
      if (length(cons) == 1L) "one construct" else sprintf("%d constructs", length(cons)),
      paste(con_txt, collapse = "; ")
    ),
    "",
    sprintf(paste0(
      "Models were estimated in R with lavaan (Rosseel, 2012) via the SIEVER ",
      "environment, using %s. Latent variables were identified by fixing each ",
      "factor variance to one, so all loadings were freely estimated. %s%s"
    ), est_txt, rel$method, sieve_draft_scores_text(items, scores)),
    "",
    "## Results (draft; verify and edit before use)",
    ""
  )
  # The correlated-factors CFA and its fit, behind the MLR memory refusal of
  # the model tabs (sieve_spec_mlr_guard()): a fit the CFA page would refuse
  # is refused here too, before the engine starts, and the draft says so in
  # place of fit indices.
  cfa_spec <- sieve_cfa_spec(meta, reserved = names(items), estimator = estimator)
  refusal <- sieve_spec_mlr_guard(cfa_spec, n = nrow(items))
  fit <- if (is.null(refusal)) {
    tryCatch(sieve_fit_cfa(items, meta, estimator = estimator), error = function(e) NULL)
  }
  lines <- c(lines, sieve_draft_provenance(fit, revision, spec), "")
  # The RMSEA interval at the RMSEA level (90% by default, DMAR's and
  # lavaan's), not at the draft's confidence level (the Reliability tab's;
  # 0.95 by default), which sets its omega and H intervals.
  # The unbiased SRMR with its interval at the same level beside the SRMR
  # (DMAR::sem_residual_fit(); see sieve_fit_usrmr()), computed whatever its
  # estimated time: the draft is asked for, and it does not follow the run
  # policy of the pages. A value, or DMAR's refusal, the CFA tab computed
  # for the identical fit (the same model, data, respondents and engines) is
  # taken from its record instead of being computed again
  # (sieve_draft_usrmr_held()), and
  # the draft names that record.
  m <- if (!is.null(fit)) sieve_fit_measures(fit, conf_level = sieve_default_rmsea_conf_level()) else NULL
  held_usrmr <- NULL
  if (!is.null(m) && isTRUE(unbiased_srmr)) {
    level <- sieve_fit_ci_level(m)
    if (!is.finite(level)) level <- sieve_default_rmsea_conf_level()
    held_usrmr <- sieve_draft_usrmr_held(analyses, attr(fit, "sieve_spec"), revision, level)
    m <- if (is.null(held_usrmr)) {
      sieve_fit_with_usrmr(m, fit, compute = "always")
    } else {
      sieve_fit_set_usrmr(m, held_usrmr$usrmr)
    }
  }
  if (!is.null(m) && is.finite(m["cfi"])) {
    # Every index is named with its family (robust, scaled, or standard):
    # the three are different statistics, and a reader comparing this report
    # with another needs to know which one each value is.
    fam <- sieve_fit_family(m, "index")
    test_fam <- sieve_fit_family(m, "test")
    lines <- c(
      lines, sprintf(
        paste0(
          "The correlated-factors confirmatory factor analysis fit as follows: ",
          "%s chi square(%d) = %.2f, p %s; %s RMSEA = %.3f, %s [%.3f, %.3f]; ",
          "%s CFI = %.3f; %s TLI = %.3f; SRMR = %.3f%s. %s [Interpret each index with its interval ",
          "and the residual correlations; no cutoff makes a model acceptable.]"
        ),
        test_fam, as.integer(m["df"]), m["chisq"],
        if (is.finite(m["pvalue"]) && m["pvalue"] < .001) "< .001" else sprintf("= %.3f", m["pvalue"]),
        fam, m["rmsea"], sieve_rmsea_ci_words(sieve_fit_ci_level(m)), m["rmsea_lo"], m["rmsea_hi"],
        fam, m["cfi"], fam, m["tli"], m["srmr"],
        if (is.null(sieve_usrmr_words(m))) "" else sprintf(" (%s)", sieve_usrmr_words(m)),
        # The note on the unbiased SRMR is the draft's own, one account of
        # it whether it was computed here, taken from the CFA tab's record,
        # refused by DMAR or left out (sieve_draft_usrmr_note()).
        paste(c(
          sieve_fit_note(sieve_fit_drop_usrmr(m)),
          sieve_draft_usrmr_note(m, held_usrmr, unbiased_srmr)
        ), collapse = " ")
      ),
      ""
    )
    # A fit index describes the solution lavaan reached; say so when that
    # solution is improper, rather than reporting the indices alone.
    if (identical(sieve_status_of(fit)$code, "inadmissible")) {
      lines <- c(lines, sprintf(
        "[This solution is improper and should not be reported as it stands. %s]",
        sieve_status_of(fit)$message
      ), "")
    }
  } else if (!is.null(refusal)) {
    lines <- c(
      lines,
      sprintf("The correlated-factors CFA was not fit for this draft. %s", refusal),
      ""
    )
  } else {
    lines <- c(
      lines,
      sprintf(
        paste0(
          "The correlated-factors CFA could not be fit on the current active items",
          "%s; resolve that before drafting results."
        ),
        if (is.null(fit)) "" else paste0(" (", sub("[.]$", "", sieve_status_of(fit)$message), ")")
      ),
      ""
    )
  }

  lines <- c(
    lines,
    "Reliability (Coefficient Omega, \u03c9, with a confidence interval; Kelley & Pornprasertmanit, 2016):",
    "", paste0("- ", rel$lines), "",
    paste0(
      "[Add: the item-retention narrative (what was excluded and why), validity ",
      "evidence (construct correlations, AVE, HTMT, criterion relations), ",
      "invariance conclusions with any freed parameters disclosed, and the ",
      "cross-validation results. The reproducible code log holds the exact ",
      "recorded inputs of each step that saved them and marks a step that did not.]"
    ),
    "",
    # The references are the methods this draft computed with (the
    # registry of fct_references.R): the estimator family and its missing
    # data treatment, the fit indices when the CFA was fit, the reliability
    # coefficients and the interval methods actually used (a bootstrap only
    # when a completed run was cited), and the software with its installed
    # versions. Page ranges are written "--", which the markdown renders as
    # an en dash. Green and Yang (2009) appears only when omega for ordered
    # items with their model-implied denominator is reported, because only
    # then was that method used.
    sieve_reference_lines(
      c(
        "reporting",
        sieve_spec_methods(
          if (is.null(fit)) list(kind = "cfa", estimator = spec$estimator, missing = spec$missing) else attr(fit, "sieve_spec"),
          estimates = if (is.null(m)) NULL else list(fit = m), fitted = !is.null(m)
        ),
        rel$keys
      ),
      markdown = TRUE, heading = "References for This Draft",
      intro = paste(
        "The methods this draft computed with and the software it ran in, with the versions",
        "installed; a method the draft did not use is not cited."
      )
    )
  )
  paste(lines, collapse = "\n")
}


#' The estimator sentence of the draft, with its treatment of incomplete
#' cases
#'
#' Says which estimator the CFA used and how incomplete cases entered it, as
#' SIEVER set it: full information maximum likelihood with MLR and with the
#' chosen ML with FIML (whose standard errors and test statistic are normal
#' theory, as the sentence says), complete cases otherwise. Complete cases
#' is SIEVER's setting for ML on the complete cases and for the categorical
#' estimator, not a requirement of the method (lavaan offers pairwise
#' deletion with WLSMV).
#'
#' @param estimator `"MLR"`, `"ML"`, or `"WLSMV"`.
#' @param missing `"fiml"` or `"listwise"`.
#' @return A phrase.
#' @author Ken Kelley
#' @noRd
sieve_draft_estimator_text <- function(estimator, missing) {
  name <- switch(estimator,
    MLR = paste0(
      "robust maximum likelihood (MLR: maximum likelihood estimates with Huber-White ",
      "standard errors and a Yuan-Bentler scaled test statistic)"
    ),
    ML = "maximum likelihood (ML), with every item treated as continuous",
    paste0(
      "weighted least squares with mean- and variance-adjusted statistics (WLSMV) on the ",
      "polychoric/tetrachoric correlations"
    )
  )
  cases <- if (identical(missing, "fiml")) {
    paste0(
      "with full information maximum likelihood, so incomplete cases contribute to the CFA",
      if (identical(estimator, "ML")) {
        ", and normal-theory standard errors and test statistic (without MLR's robust corrections)"
      }
    )
  } else if (identical(estimator, "WLSMV")) {
    paste0(
      "on the complete cases (listwise deletion, SIEVER's setting for the categorical ",
      "estimator; lavaan also offers pairwise deletion)"
    )
  } else {
    "on the complete cases (listwise deletion, SIEVER's setting for this estimator)"
  }
  paste(name, cases, sep = ", ")
}

#' The reasons respondents were removed, counted, for the draft
#'
#' @param removed A `sieve_removed_table()` result.
#' @return A phrase such as `"2 for failed attention check; 1 with no reason
#'   recorded"`. Identifiers are not part of it.
#' @author Ken Kelley
#' @noRd
sieve_draft_reasons <- function(removed) {
  reason <- trimws(as.character(removed$reason))
  reason[is.na(reason) | !nzchar(reason)] <- NA_character_
  given <- reason[!is.na(reason)]
  counts <- if (length(given)) table(factor(given, levels = unique(given))) else integer(0)
  parts <- c(
    if (length(counts)) sprintf("%d for %s", as.integer(counts), names(counts)),
    if (anyNA(reason)) sprintf("%d with no reason recorded", sum(is.na(reason)))
  )
  paste(parts, collapse = "; ")
}

#' The item treatment the Reliability tab resolves
#'
#' The tab's `"study"` choice (its default) reads the study's estimator
#' setting: every item continuous for MLR, ML with FIML or ML, every item ordered for
#' WLSMV, and the item types otherwise. This is the rule of the tab's
#' treatment control (`mod_reliab.R`), so the draft and the tab resolve the
#' same treatment.
#'
#' @param treatment The tab's setting, or `NULL`.
#' @param estimator The study's estimator setting.
#' @return One of `sieve_reliability_treatments()`.
#' @author Ken Kelley
#' @noRd
sieve_draft_treatment <- function(treatment, estimator = "auto") {
  t <- treatment %||% "study"
  if (identical(t, "study")) {
    t <- switch(sieve_estimator_effective(estimator),
      mlr = ,
      ml_fiml = ,
      ml = "continuous",
      wlsmv = "ordered",
      "auto"
    )
  }
  if (t %in% sieve_reliability_treatments()) t else "auto"
}

#' What a resolved reliability request estimates, in words
#'
#' The labels DMAR's definition of robust omega sets: "Robust omega (observed
#' denominator)" and "Omega (model-implied denominator)", never an unqualified
#' omega beside a value; for ordered items, the matrix the denominator is
#' built from is named, and so is the composite the coefficient scores, the
#' sum of category ranks (see `sieve_reliability_coding()`).
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @return A phrase naming the coefficient and its denominator.
#' @author Ken Kelley
#' @noRd
sieve_draft_coefficient <- function(spec) {
  # The labels of DMAR's definition: never an unqualified "omega" beside a
  # value; for ordered items the observed denominator is built from the
  # unrestricted polychoric correlations.
  base <- switch(spec$coefficient,
    omega_robust = "Robust omega (observed denominator)",
    omega_categorical = if (identical(spec$denominator, "model_implied")) {
      "Omega for ordered items (model-implied denominator; the reliability of the sum of category ranks)"
    } else {
      paste0(
        "Robust omega for ordered items (observed denominator, built from the unrestricted polychoric ",
        "correlations; the reliability of the sum of category ranks)"
      )
    },
    "Omega (model-implied denominator)"
  )
  if (identical(spec$item_class, "mixed")) {
    base <- sub(")$", "; ordered items enter as their numeric codes)", base)
  }
  base
}

#' A completed bootstrap of the same coefficient on the same data
#'
#' The registry is searched for a reliability record whose revision is the
#' draft's (the same dataset, item data, ledger and respondents) and that
#' holds a group with the draft's items, item treatment, coefficient,
#' missing data treatment and confidence level, whose interval is a
#' completed, complete bootstrap. The latest such record is used.
#'
#' @param analyses The analysis registry.
#' @param spec The draft's specification of the group.
#' @param revision The draft's revision.
#' @return `NULL`, or a list with `id`, `time`, and `omega` (the recorded
#'   coefficient with its interval).
#' @author Ken Kelley
#' @noRd
sieve_draft_bootstrap <- function(analyses, spec, revision) {
  hit <- NULL
  same <- function(a, b) identical(as.character(a), as.character(b))
  for (rec in analyses %||% list()) {
    if (!is.list(rec) || !identical(rec$kind, "reliability") ||
      !is.list(rec$revision) || !sieve_same_revision(rec$revision, revision)) {
      next
    }
    groups <- rec$spec$groups %||% list()
    for (i in seq_along(groups)) {
      g <- groups[[i]]
      if (!same(g$items, spec$items) || !same(g$types, spec$types) ||
        !same(g$coefficient, spec$coefficient) || !same(g$missing, spec$missing) ||
        !isTRUE(all.equal(as.numeric(g$conf_level), as.numeric(spec$conf_level)))) {
        next
      }
      om <- rec$estimates[[i]]$omega
      u <- om$uncertainty
      boot <- isTRUE(u$method %in% c(sieve_reliability_bootstrap_methods(), "bca", "percentile")) ||
        grepl("bootstrap", tolower(u$label %||% ""), fixed = TRUE)
      if (is.list(om) && identical(u$status, "completed") && !isFALSE(u$complete) && boot &&
        is.finite(om$lower) && is.finite(om$upper)) {
        hit <- list(id = rec$id, time = rec$time, omega = om)
      }
    }
  }
  hit
}

#' The unbiased SRMR the CFA tab computed for the draft's own fit
#'
#' The registry is searched for a CFA record whose revision is the draft's
#' (the same dataset, item data, ledger and respondents), whose
#' specification is the draft's in everything a fit's identity takes from
#' it (`sieve_fit_spec_identity()`: the model syntax, the ordered
#' indicators, the estimator, the missing data treatment, the
#' identification, every further engine argument and the sampling design),
#' fit under the same lavaan and DMAR installations, and whose fit measures
#' hold an unbiased SRMR that was computed (a value, or DMAR's refusal) at
#' the draft's level. Such a record is of the identical fit, so its value,
#' or DMAR's refusal with its reason, is what the draft would compute; the
#' latest is used, and the draft says which it is
#' (`sieve_draft_usrmr_note()`). A record whose value was left for its
#' button is not used.
#'
#' @param analyses The analysis registry, or `NULL`.
#' @param spec The specification of the draft's fit
#'   (`attr(fit, "sieve_spec")`).
#' @param revision The draft's revision.
#' @param conf_level The level of the draft's RMSEA interval.
#' @return `NULL`, or a list with `id` (the record) and `usrmr` (a result
#'   of `sieve_fit_usrmr()`).
#' @author Ken Kelley
#' @noRd
sieve_draft_usrmr_held <- function(analyses, spec, revision, conf_level) {
  if (!length(analyses) || is.null(spec) || !is.list(revision)) {
    return(NULL)
  }
  own <- sieve_fit_spec_identity(spec)
  engines <- function(identity) {
    keys <- tryCatch(sieve_engine_keys(identity), error = function(e) character(0))
    keys[c("lavaan", "DMAR")]
  }
  current <- NULL
  hit <- NULL
  for (rec in analyses) {
    if (!is.list(rec) || !identical(rec$kind, "cfa") || !is.list(rec$revision) ||
      !sieve_same_revision(rec$revision, revision) || !is.list(rec$spec) ||
      !identical(sieve_fit_spec_identity(rec$spec), own)) {
      next
    }
    u <- attr(rec$estimates$fit, "families")$usrmr
    if (!is.list(u) || !isTRUE(u$computed) ||
      !isTRUE(all.equal(as.numeric(u$conf_level), as.numeric(conf_level)))) {
      next
    }
    # The installations are read only for a record that matches otherwise.
    current <- current %||% engines(sieve_engine_identity(c("lavaan", "DMAR")))
    if (!anyNA(current) && identical(engines(rec$engine_identity), current)) {
      hit <- list(id = rec$id, usrmr = u)
    }
  }
  hit
}

#' What the methods draft says of its unbiased SRMR
#'
#' One account of the unbiased SRMR beside the draft's SRMR, so the draft
#' never says both that a value was taken from a record and that none is
#' reported. Computed by the draft: what the value is
#' (`sieve_usrmr_note()`), or DMAR's reason when DMAR refused it. Taken
#' from the CFA tab's record of the identical fit
#' (`sieve_draft_usrmr_held()`): the same, with that record named in the
#' sentence that says where the value, or the refusal, came from. Not
#' computed: only when `unbiased_srmr = FALSE` left it out.
#'
#' @param m The draft's fit measures.
#' @param held A result of `sieve_draft_usrmr_held()`, or `NULL`.
#' @param requested The draft's `unbiased_srmr`.
#' @return A character vector of sentences (empty when the unbiased SRMR
#'   was asked for and the measures hold none).
#' @author Ken Kelley
#' @noRd
sieve_draft_usrmr_note <- function(m, held = NULL, requested = TRUE) {
  if (!isTRUE(requested)) {
    return("The unbiased SRMR and its interval were not computed for this draft (unbiased_srmr = FALSE).")
  }
  u <- attr(m, "families")$usrmr
  if (is.null(u)) {
    return(character(0))
  }
  if (is.null(held)) {
    return(sieve_usrmr_note(u) %||% character(0))
  }
  same <- sprintf("this identical fit (the same model, data, respondents and engines; analysis %s)", held$id)
  if (!isTRUE(u$available)) {
    return(sprintf(
      paste0(
        "The unbiased SRMR and its interval are not reported for this fit, for the reason DMAR gave when ",
        "the CFA tab asked for them for %s, taken from that record: %s"
      ),
      same, u$reason
    ))
  }
  c(
    sieve_usrmr_note(u),
    sprintf("The unbiased SRMR and its interval are those the CFA tab computed for %s, taken from that record.", same)
  )
}

#' The unbiased SRMR of a methods draft, before it is assembled
#'
#' What the draft's button can say before it is pressed: the number of
#' observed variables of the draft's correlated-factors CFA and the
#' estimated time of its unbiased SRMR (`sieve_usrmr_seconds()`), whether
#' the CFA tab already computed it for the identical fit
#' (`sieve_draft_usrmr_held()`), and whether DMAR will refuse it at once or
#' there will be no fit to compute it for (a specification problem, the
#' memory refusal of the model tabs, an estimator other than ML or MLR,
#' ordered items, or incomplete data under full information maximum
#' likelihood; see `sieve_usrmr_refused()`). Nothing is fit.
#'
#' @param items,meta,estimator,revision,analyses As for
#'   [sieve_methods_draft()].
#' @return A list with `p`, `seconds`, `held` (the identifier of the record
#'   whose value the draft will take, or `NULL`), `refused` and `slow`
#'   (`TRUE` when the draft will compute it and its estimated time exceeds
#'   the run policy's `sieve_auto_seconds()`).
#' @author Ken Kelley
#' @noRd
sieve_draft_usrmr_plan <- function(items, meta, estimator = "auto", revision = NULL, analyses = NULL) {
  out <- list(p = 0L, seconds = NA_real_, held = NULL, refused = TRUE, slow = FALSE)
  if (is.null(items) || !is.data.frame(meta) || !nrow(meta)) {
    return(out)
  }
  spec <- tryCatch(sieve_cfa_spec(meta, reserved = names(items), estimator = estimator), error = function(e) NULL)
  if (is.null(spec) || length(spec$problems) || !nzchar(spec$model %||% "")) {
    return(out)
  }
  observed <- intersect(spec$observed, names(items))
  out$p <- length(spec$observed)
  out$seconds <- sieve_usrmr_seconds(out$p)
  out$held <- sieve_draft_usrmr_held(analyses, spec, revision, sieve_default_rmsea_conf_level())$id
  out$refused <- !is.null(sieve_spec_mlr_guard(spec, n = nrow(items))) ||
    length(spec$ordered) > 0L || !isTRUE(spec$estimator %in% c("ML", "MLR")) ||
    (identical(spec$missing, "fiml") && anyNA(items[, observed, drop = FALSE]))
  out$slow <- is.null(out$held) && !out$refused && is.finite(out$seconds) &&
    out$seconds > sieve_auto_seconds()
  out
}

#' The label of the methods draft's button
#'
#' States the estimated time of the unbiased SRMR when the draft will
#' compute it and that estimate exceeds the run policy's two seconds, as
#' every button of a computation that does not run by itself does.
#'
#' @param plan A result of `sieve_draft_usrmr_plan()`, or `NULL`.
#' @return A string.
#' @author Ken Kelley
#' @noRd
sieve_draft_button_label <- function(plan) {
  if (is.null(plan) || !isTRUE(plan$slow)) {
    return("Run a new analysis and draft")
  }
  sprintf("Run a new analysis and draft (the unbiased SRMR alone: %s)", sieve_duration_label(plan$seconds))
}

#' The draft's reliability: the Reliability tab's default coefficient
#'
#' One request per construct (or facet, by the tab's reporting scope), for
#' the coefficient the Reliability tab reports by default (robust omega for
#' continuous items and categorical omega for ordered or binary items, with
#' DMAR's default intervals: the bootstrap for each, which the draft does
#' not run), on the items as the tab's item treatment takes them, with the study estimator's
#' treatment of incomplete cases (full information maximum likelihood with
#' ML with FIML, the default for continuous items, and with MLR; complete
#' cases otherwise; categorical omega always uses complete
#' cases, and says so). The request is resolved by the Reliability tab's
#' own functions: `sieve_reliability_spec()` with `ci_method =
#' "recommended"`, and
#' [sieve_reliability_construct()]. A bootstrap interval is not run here:
#' it is taken from a completed run on the same data
#' (`sieve_draft_bootstrap()`) and is otherwise reported as not run. Beside
#' robust omega, model implied omega with DMAR's default interval is
#' reported, labeled as assuming the one-factor model, with that model's
#' RMSEA where the functions return it. Each line states the coefficient's
#' case treatment and the n it used.
#'
#' @param items,meta The item data and the ledger.
#' @param settings The Reliability tab's settings, or `NULL` (its item
#'   treatment, reporting scope and confidence level are read).
#' @param estimator The study's estimator setting.
#' @param revision The draft's revision.
#' @param analyses The analysis registry, or `NULL`.
#' @param missing The study estimator's treatment of incomplete cases,
#'   `"fiml"` (the default, DMAR's) or `"listwise"`.
#' @return A list with `lines` (one per group), `method` (the sentence for
#'   the method paragraph), `green_yang` (whether omega for ordered items
#'   with its model-implied denominator was reported), and `keys` (the
#'   reference registry keys of the methods the lines report: each
#'   coefficient, the missing data treatment, the interval methods actually
#'   used, and the companion coefficient).
#' @author Ken Kelley
#' @noRd
sieve_draft_reliability <- function(items, meta, settings, estimator, revision, analyses = NULL,
                                    missing = "fiml") {
  resolved <- sieve_draft_reliability_specs(items, meta, settings, estimator, missing)
  treatment <- resolved$treatment
  scope <- resolved$scope
  conf <- resolved$conf
  request <- resolved$request
  groups <- resolved$groups
  specs <- resolved$specs
  per_group <- lapply(seq_along(groups), function(k) {
    g <- groups[[k]]
    sp <- specs[[k]]
    keys <- c(
      switch(sp$coefficient,
        omega_robust = c("omega_robust", "omega_numerator"),
        omega_categorical = c(
          if (identical(sp$denominator, "model_implied")) "omega_categorical_model_implied" else "omega_categorical",
          "omega_numerator"
        ),
        c("omega", "omega_numerator")
      ),
      if (identical(sp$missing, "fiml")) "fiml"
    )
    res <- sieve_reliability_construct(items, sp, alpha = FALSE, model_implied = TRUE, meta = meta)
    r <- res$omega
    what <- sieve_draft_coefficient(sp)
    if (!is.finite(r$estimate)) {
      return(list(
        line = sprintf("%s: %s could not be estimated (%s).", g$construct, what, r$status$message),
        keys = character(0)
      ))
    }
    cases <- if (identical(sp$missing, "fiml")) {
      sprintf("full information maximum likelihood, n = %d of %d", r$n$used, r$n$rows)
    } else {
      sprintf("complete cases on its items (listwise deletion), n = %d of %d", r$n$used, r$n$rows)
    }
    pct <- format(100 * sp$conf_level)
    interval <- if (is.finite(r$lower) && is.finite(r$upper)) {
      keys <- c(keys, switch(as.character(r$uncertainty$method %||% ""), mlr = , mlr_logistic = "mlr",
        ml = , ml_logistic = "ml", character(0)))
      sprintf("%s%% CI [%.2f, %.2f], %s", pct, r$lower, r$upper, r$uncertainty$label)
    } else if (identical(sp$uncertainty$status, "pending") ||
      sp$coefficient %in% c("omega_robust", "omega_categorical")) {
      # The interval of robust and categorical omega is a bootstrap, run on
      # the Reliability tab: a completed run on these data is reported.
      boot <- sieve_draft_bootstrap(analyses, sp, revision)
      if (!is.null(boot)) {
        u <- boot$omega$uncertainty
        keys <- c(keys, switch(as.character(u$method %||% ""),
          percentile = , dmar_percentile = , bootstrap_se_logistic = c("bootstrap_percentile", "bootstrap_guidance"),
          dmar_bca = , bca = c("bootstrap_bca", "bootstrap_guidance"),
          c("bootstrap_percentile", "bootstrap_guidance")
        ))
        sprintf(
          "%s%% CI [%.2f, %.2f], %s (%s of %s replications used), from the bootstrap run on the Reliability tab on these data (analysis %s)",
          pct, boot$omega$lower, boot$omega$upper, u$label,
          format(u$B_used %||% NA), format(u$B_attempted %||% u$B %||% NA), boot$id
        )
      } else if (identical(sp$uncertainty$status, "pending")) {
        sprintf(
          "interval: bootstrap not run yet (%s; run it on the Reliability tab, then refresh the draft)",
          sieve_reliability_interval(sp$missing, sp$ci_target %||% sp$requested$ci_method)$label
        )
      } else {
        "interval: bootstrap not run yet (its interval is a bootstrap; run it on the Reliability tab, then refresh the draft)"
      }
    } else {
      paste(
        "interval unavailable:",
        sub("[.]+$", "", r$uncertainty$reason %||% sp$uncertainty$reason %||% "not estimated")
      )
    }
    beside <- NULL
    mi <- res$model_implied
    if (is.list(mi) && is.finite(mi$estimate %||% NA_real_)) {
      keys <- c(keys, "omega", switch(as.character(mi$uncertainty$method %||% ""), mlr = , mlr_logistic = "mlr",
        ml = , ml_logistic = "ml", character(0)))
      fs <- res$H$fit_summary
      rmsea <- if (!is.null(fs)) sieve_reliability_rmsea_text(fs)
      beside <- sprintf(
        " Beside it, %s = %.2f%s%s.",
        sieve_draft_coefficient(mi$spec %||% list(coefficient = "omega")), mi$estimate,
        if (is.finite(mi$lower) && is.finite(mi$upper)) {
          sprintf(", %s%% CI [%.2f, %.2f], %s", pct, mi$lower, mi$upper, mi$uncertainty$label)
        } else {
          ""
        },
        if (!is.null(rmsea)) sprintf("; one-factor RMSEA %s", rmsea) else ""
      )
    }
    notes <- c(sp$missing_reason, sp$notes, if (!is.na(res$coding$note %||% NA_character_)) res$coding$note)
    list(
      line = sprintf(
        "%s: %s = %.2f (%s), %s.%s%s", g$construct, what, r$estimate, cases, interval,
        if (is.null(beside)) "" else beside,
        if (length(notes)) paste0(" ", paste(notes, collapse = " ")) else ""
      ),
      keys = keys
    )
  })
  lines <- vapply(per_group, function(p) p$line, character(1))
  keys <- unique(unlist(lapply(per_group, function(p) p$keys), use.names = FALSE))
  coefficients <- unique(vapply(specs, function(sp) sp$coefficient, character(1)))
  green_yang <- any(vapply(specs, function(sp) {
    identical(sp$coefficient, "omega_categorical") && identical(sp$denominator, "model_implied")
  }, logical(1)))
  named <- c(
    omega_robust = paste0(
      "Robust omega (observed denominator), Kelley and Pornprasertmanit's (2016) hierarchical omega (not ",
      "the bifactor omega hierarchical): the squared sum of the one-factor loadings over the observed ",
      "variance of the composite, so it is robust to misspecification of the composite's total variance ",
      "only (its numerator is the one-factor model's)"
    ),
    omega = paste0(
      "Omega (model-implied denominator), which takes the total variance from the one-factor model as ",
      "well and so assumes that model holds"
    ),
    omega_categorical = paste0(
      if (green_yang) {
        "Omega for ordered items (model-implied denominator; Green & Yang, 2009) for constructs of ordered or binary items"
      } else {
        paste0(
          "for constructs of ordered or binary items, Robust omega for ordered items (observed denominator, ",
          "built from the unrestricted polychoric correlations; Kelley & Pornprasertmanit, 2016)"
        )
      },
      ", the reliability of the sum of the items' category ranks (each item's observed categories scored 0, 1, ",
      "..., C - 1 in the order of its codes, as DMAR::reliability_omega_categorical() scores them), which is ",
      "the reliability of the sum of the codes the scores use only when every item's codes step by one shared ",
      "amount (a construct whose codes do not is named, with its coded composite's reliability, in its line)"
    )
  )
  words <- c(
    auto = "as typed on the Ingest tab",
    continuous = "as continuous",
    ordered = "as ordered (a binary item as binary)"
  )
  missing_used <- unique(vapply(specs, function(sp) sp$missing, character(1)))
  ci_codes <- unique(vapply(specs, function(sp) sp$ci_target %||% sp$requested$ci_method, character(1)))
  method_names <- sieve_reliability_methods()
  ci_words <- vapply(ci_codes, function(cd) {
    if (cd %in% sieve_reliability_bootstrap_methods() || cd %in% sieve_reliability_closed_methods()) {
      sieve_reliability_interval("listwise", cd)$label
    } else {
      nm <- names(method_names)[match(cd, method_names)]
      if (is.na(nm)) cd else nm
    }
  }, character(1))
  method <- if (!length(specs)) {
    "No construct had an included item, so no reliability coefficient was computed."
  } else {
    sprintf(
      paste0(
        "Reliability was estimated per %s as SIEVER's Reliability tab estimates it by default, with ",
        "DMAR's default intervals, and computed with the DMAR package: %s, with the items ",
        "treated %s (the Reliability tab's item treatment); %s; interval: %s at the %s%% level (a ",
        "bootstrap interval is reported only when it was run on the Reliability tab on these data). ",
        "The case treatment and the n of each coefficient are stated with its value."
      ),
      if (identical(scope, "construct")) "construct" else "construct and facet",
      paste(unname(named[intersect(names(named), coefficients)]), collapse = ", and "),
      words[[treatment]],
      if (identical(missing_used, "fiml")) {
        "incomplete cases entered through full information maximum likelihood, as in the CFA"
      } else if (identical(missing_used, "listwise")) {
        paste0(
          "each coefficient used the complete cases on its construct's items (listwise deletion",
          if (identical(request$missing, "fiml")) {
            ": full information maximum likelihood, used in the CFA, is not offered for ordered or binary items)"
          } else {
            ", as in the CFA)"
          }
        )
      } else {
        paste0(
          "full information maximum likelihood for continuous items, as in the CFA, and complete cases ",
          "for ordered or binary items (the engine fits them by weighted least squares)"
        )
      },
      paste(ci_words, collapse = "; "),
      format(100 * conf)
    )
  }
  list(lines = unname(lines), method = method, green_yang = green_yang, keys = keys)
}

#' The reliability requests the draft resolves, one per construct or facet
#'
#' The part of `sieve_draft_reliability()` that decides what is computed:
#' the Reliability tab's item treatment, reporting scope and confidence
#' level resolved from its settings, and one `sieve_reliability_spec()`
#' per group, with DMAR's default intervals and the study estimator's
#' treatment of incomplete cases, and no bootstrap. Computing nothing, it
#' is also what the draft's currency check reads to find a bootstrap
#' completed since the draft was assembled (`sieve_draft_bootstrap_ids()`).
#'
#' @inheritParams sieve_draft_reliability
#' @return A list with `treatment`, `scope`, `conf`, `request`, `groups`
#'   (see `sieve_reliability_items()`) and `specs` (one specification per
#'   group).
#' @author Ken Kelley
#' @noRd
sieve_draft_reliability_specs <- function(items, meta, settings, estimator, missing = "fiml") {
  s <- if (is.list(settings)) settings else list()
  treatment <- sieve_draft_treatment(s$treatment, estimator)
  scope <- if (length(s$scope) == 1L && s$scope %in% c("construct", "facet", "combined")) s$scope else "construct"
  conf <- suppressWarnings(as.numeric(s$conf_level %||% 0.95))
  if (length(conf) != 1L || !is.finite(conf) || conf <= 0 || conf >= 1) conf <- 0.95
  request <- list(missing = if (identical(missing, "fiml")) "fiml" else "listwise", conf_level = conf,
    ci_method = "recommended")
  groups <- sieve_reliability_items(meta, names(items), scope = scope)
  specs <- lapply(groups, function(g) {
    do.call(sieve_reliability_spec, c(
      list(
        items = g$items, types = sieve_reliability_treat(g$types, treatment), construct = g$construct,
        run_bootstrap = FALSE, group_id = g$id, scope = g$scope, parent_construct = g$parent_construct,
        facet = g$facet
      ),
      request
    ))
  })
  list(treatment = treatment, scope = scope, conf = conf, request = request, groups = groups, specs = specs)
}

#' The completed bootstrap records the draft would cite, by group
#'
#' @inheritParams sieve_draft_reliability
#' @return A named character vector, one element per group the draft
#'   reports (named by the group's construct, or construct and facet): the
#'   identifier of the completed bootstrap record of that group's
#'   coefficient on these data (see `sieve_draft_bootstrap()`), or `NA`.
#' @author Ken Kelley
#' @noRd
sieve_draft_bootstrap_ids <- function(items, meta, settings, estimator, revision, analyses = NULL,
                                      missing = "fiml") {
  resolved <- tryCatch(
    sieve_draft_reliability_specs(items, meta, settings, estimator, missing),
    error = function(e) NULL
  )
  if (is.null(resolved) || !length(resolved$specs)) {
    return(stats::setNames(character(0), character(0)))
  }
  ids <- vapply(resolved$specs, function(sp) {
    if (!sp$coefficient %in% c("omega_robust", "omega_categorical")) {
      return(NA_character_)
    }
    hit <- sieve_draft_bootstrap(analyses, sp, revision)
    if (is.null(hit)) NA_character_ else as.character(hit$id)
  }, character(1))
  names(ids) <- vapply(resolved$groups, function(g) {
    if (length(g$facet) == 1L && !is.na(g$facet) && nzchar(g$facet) && !identical(g$facet, g$construct)) {
      paste(g$parent_construct %||% g$construct, g$facet, sep = ": ")
    } else {
      as.character(g$construct)
    }
  }, character(1))
  ids
}

#' Everything the methods draft reads, as a snapshot
#'
#' The draft computes from the item data and the ledger, and also from the
#' study's estimator setting, the sampling design, three fields of the
#' Reliability tab's settings (its item treatment, reporting scope and
#' confidence level) and the completed bootstrap records it cites. A draft
#' is current only while all of them are as they were when it was
#' assembled, so this snapshot is kept with the draft and compared with
#' the one taken now (`sieve_draft_stale_reason()`).
#'
#' @param state The shared state store (read under [shiny::isolate()]).
#' @return A list with `revision` (see `sieve_revision()`), `estimator`
#'   (one of `sieve_estimator_choices()`), `sampling` (the declared unit,
#'   the cluster column and the content key of its values), `reliability`
#'   (`treatment`, `scope`, `conf_level`) and `bootstraps` (see
#'   `sieve_draft_bootstrap_ids()`).
#' @author Ken Kelley
#' @noRd
sieve_draft_inputs <- function(state) {
  shiny::isolate({
    items <- state$items
    meta <- state$meta
    estimator <- sieve_estimator_effective("study", state)
    design <- tryCatch(sieve_sampling_spec(state), error = function(e) list(unit = "unsupported", cluster = NULL))
    column <- design$cluster
    raw <- state$raw
    if (length(column) == 1L && !is.na(column) && nzchar(column) && column %in% names(raw)) {
      design$values_key <- sieve_hash(raw[[column]])
    }
    s <- state$reliability_settings
    s <- if (is.list(s)) s else list()
    reliability <- list(
      treatment = s$treatment %||% "study", scope = s$scope %||% "construct",
      conf_level = suppressWarnings(as.numeric(s$conf_level %||% 0.95))
    )
    revision <- sieve_revision(state)
    bootstraps <- if (!is.null(items) && is.data.frame(meta) && nrow(meta)) {
      missing <- tryCatch(sieve_lavaan_spec(meta, estimator = estimator)$missing, error = function(e) "fiml")
      sieve_draft_bootstrap_ids(items, meta, s, estimator, revision, state$analyses, missing = missing)
    } else {
      stats::setNames(character(0), character(0))
    }
    list(
      revision = revision, estimator = estimator, sampling = design,
      reliability = reliability, bootstraps = bootstraps,
      # The operational score specification the draft's method paragraph
      # states (sieve_draft_scores_text()).
      scores = sieve_hash(state$scoring_policies %||% list())
    )
  })
}

#' The operational scores, in one sentence of the draft
#'
#' One clause per construct from the operational score specification
#' (`sieve_score_specification()`): a composite is named by its method, its
#' minimum answered rule and the number of respondents who meet it on the
#' item data (`sieve_composite_score()`); a factor score by its method from
#' the fitted measurement model. A construct whose stored policy could not
#' be used as saved is said to be provisional, with the method as stored, so
#' the draft never states a repair as the specification.
#'
#' @param items The item data.
#' @param scores The specification, or `NULL` (no sentence).
#' @return A sentence beginning with a space, or `""`.
#' @author Ken Kelley
#' @noRd
sieve_draft_scores_text <- function(items, scores) {
  if (!length(scores)) {
    return("")
  }
  provisional <- function(s) {
    if (is.null(s$issue)) {
      return("")
    }
    sprintf(
      " (provisional: the stored policy%s could not be used as saved, and this is in force until a method is kept)",
      if (!is.na(s$requested_method %||% NA_character_)) sprintf(", method \"%s\",", s$requested_method) else ""
    )
  }
  parts <- vapply(scores, function(s) {
    if (isTRUE(s$latent)) {
      return(sprintf("%s, %s from the fitted measurement model%s", s$construct, tolower(s$label %||% s$method), provisional(s)))
    }
    eligible <- tryCatch(
      sum(sieve_composite_score(items, s$items,
        weights = s$weights, method = s$method %||% "mean",
        min_count = s$min_count %||% 1L, min_prop = s$min_prop %||% 0
      )$eligible, na.rm = TRUE),
      error = function(e) NA_integer_
    )
    sprintf(
      "%s, the %s of the answered items with at least %d of %d answered%s%s",
      s$construct, if (identical(s$method, "sum")) "prorated sum" else "mean",
      as.integer(s$required %||% s$min_count %||% 1L), length(s$items),
      if (is.finite(eligible)) sprintf(" (n = %d eligible)", as.integer(eligible)) else "",
      provisional(s)
    )
  }, character(1))
  sprintf(" Operational scores, as specified on the Reliability tab: %s.", paste(parts, collapse = "; "))
}

#' An estimator choice, in the words of the Estimator control
#'
#' @param choice One of `sieve_estimator_choices()`.
#' @return A short phrase.
#' @author Ken Kelley
#' @noRd
sieve_draft_estimator_word <- function(choice) {
  switch(as.character(choice %||% "auto"),
    auto = "following the item types",
    mlr = "MLR with FIML",
    ml_fiml = "ML with FIML",
    ml = "ML on the complete cases",
    wlsmv = "WLSMV",
    as.character(choice)
  )
}

#' Why a methods draft is out of date with the analysis
#'
#' Compares the snapshot a draft was assembled from with the snapshot now
#' (`sieve_draft_inputs()`) and names the first thing that changed, in
#' plain words: the dataset, the item data or the ledger (as
#' `sieve_stale_reason()` says it), the study estimator, the sampling
#' design, a Reliability tab setting the draft follows, or a bootstrap
#' interval completed since for a construct the draft reports.
#'
#' @param made The snapshot the draft was assembled from.
#' @param now The snapshot now.
#' @return `NULL` while the draft is current, otherwise one sentence
#'   fragment naming what changed.
#' @author Ken Kelley
#' @noRd
sieve_draft_stale_reason <- function(made, now) {
  if (is.null(made)) {
    return(NULL)
  }
  why <- sieve_stale_reason(made$revision, now$revision)
  if (!is.null(why)) {
    return(why)
  }
  if (!identical(made$estimator, now$estimator)) {
    return(sprintf(
      "the study estimator changed (%s then, %s now)",
      sieve_draft_estimator_word(made$estimator), sieve_draft_estimator_word(now$estimator)
    ))
  }
  if (!identical(made$sampling, now$sampling)) {
    then_unit <- made$sampling$unit %||% "independent"
    now_unit <- now$sampling$unit %||% "independent"
    return(if (!identical(then_unit, now_unit)) {
      sprintf("the sampling design changed (%s rows then, %s now)", then_unit, now_unit)
    } else {
      "the sampling design changed (the cluster column or its values)"
    })
  }
  then_r <- made$reliability %||% list()
  now_r <- now$reliability %||% list()
  if (!identical(then_r$treatment, now_r$treatment)) {
    return("the Reliability tab's item treatment changed")
  }
  if (!identical(then_r$scope, now_r$scope)) {
    return("the Reliability tab's reporting scope changed")
  }
  if (!isTRUE(all.equal(as.numeric(then_r$conf_level), as.numeric(now_r$conf_level)))) {
    return("the Reliability tab's confidence level changed")
  }
  if (!identical(made$scores, now$scores)) {
    return("the operational score specification changed")
  }
  then_b <- made$bootstraps %||% character(0)
  now_b <- now$bootstraps %||% character(0)
  completed <- names(now_b)[!is.na(now_b) & (is.na(then_b[names(now_b)]) | then_b[names(now_b)] != now_b)]
  if (length(completed)) {
    return(sprintf(
      "a bootstrap interval for %s was completed since (analysis %s)",
      sieve_tab_list_text(completed), paste(unique(now_b[completed]), collapse = ", ")
    ))
  }
  NULL
}

#' What the draft computed, and from which data
#'
#' The paragraph at the head of the results that says what model the
#' numbers describe and which revision of the data and ledger produced
#' them, so the draft is not read as a transcript of the session.
#'
#' @param fit The CFA the draft refit (a lavaan object carrying its
#'   specification), or `NULL` when it could not be fit.
#' @param revision A list with `dataset_id`, `data_key`, and `meta_key`.
#' @param spec The estimator specification from `sieve_lavaan_spec()`, used
#'   to name the estimator when there is no fit to read it from.
#' @return A character vector of markdown lines.
#' @author Ken Kelley
#' @noRd
sieve_draft_provenance <- function(fit, revision, spec) {
  fitted <- if (is.null(fit)) NULL else attr(fit, "sieve_spec")
  estimator <- if (!is.null(fitted)) fitted$estimator else spec$estimator %||% (if (is.null(spec$ordered)) "ML" else "WLSMV")
  missing <- if (!is.null(fitted)) fitted$missing else spec$missing %||% (if (is.null(spec$ordered)) "fiml" else "listwise")
  missing_txt <- if (identical(missing, "fiml")) {
    "full information maximum likelihood for incomplete cases"
  } else {
    "listwise deletion"
  }
  key <- function(x) if (is.null(x) || is.na(x)) "not recorded" else as.character(x)
  c(
    sprintf(
      paste0(
        "What this draft computed. Every number below was computed when the draft ",
        "was assembled, from the item data and the ledger as they stood at that ",
        "moment (dataset %s; data revision %s; ledger revision %s), apart from the two ",
        "exceptions below. The ",
        "correlated-factors CFA was refit for the draft with the study's estimator ",
        "(every included item on its assigned construct, factor ",
        "variances fixed to one, %s with %s), and reliability was recomputed per construct ",
        "from the same data as the Reliability tab computes it by default. The draft does ",
        "not read the results shown on the CFA or other tabs, with two exceptions, each taken ",
        "from a record of the same computation on these data and named where it appears: a ",
        "bootstrap interval the Reliability tab ran for the same request, and the unbiased ",
        "SRMR, or DMAR's refusal of it, the CFA tab computed for this identical fit (the same ",
        "model, data, respondents and engines). It does not reproduce an analysis run ",
        "earlier in the session with other settings or another item pool; the ",
        "reproducible code script records those as they were run."
      ),
      key(revision$dataset_id), key(revision$data_key), key(revision$meta_key),
      estimator, missing_txt
    ),
    if (!is.null(fitted) && nzchar(fitted$model)) {
      c(
        "", "The model refit for this draft:", "",
        paste0("    ", strsplit(trimws(fitted$model), "\n", fixed = TRUE)[[1]])
      )
    }
  )
}

#' Report selected saved analyses without refitting
#' @param state Shared state.
#' @param ids Analysis identifiers from the registry.
#' @return Character report text.
#' @author Ken Kelley
#' @noRd
sieve_saved_analyses_report <- function(state, ids) {
  records <- shiny::isolate(state$analyses) %||% list()
  lines <- c(
    "SIEVER saved analysis report", "These are recorded results. No model is refit to prepare this report.",
    "Fit and reliability do not establish validity; interpret evidence for specified score uses.", ""
  )
  now <- sieve_revision(state)
  for (id in unique(as.character(ids))) {
    # A requested record that is not in the registry is listed as absent;
    # it is never refit in its place.
    if (!id %in% names(records)) {
      lines <- c(lines, sieve_record_absent_lines(id), "")
      next
    }
    r <- records[[id]]
    lines <- c(
      lines, paste("Analysis", id, r$note %||% r$kind),
      paste("Status:", r$status$code, r$status$message),
      paste("Recorded:", r$time %||% "time not recorded"),
      sieve_record_currency(r, state, now),
      paste("Input revision:", paste(unlist(r$revision), collapse = "; ")),
      "Exact specification:", utils::capture.output(dput(r$spec)),
      "Unrounded stored results:",
      if (is.null(r$estimates)) "No compact results stored with this record; no replacement analysis was run." else utils::capture.output(dput(r$estimates,
        control = c(
          "keepNA",
          "keepInteger",
          "niceNames",
          "showAttributes",
          "digits17"
        )
      )),
      if (length(r$warnings)) paste("Engine condition:", r$warnings), ""
    )
  }
  paste(lines, collapse = "\n")
}

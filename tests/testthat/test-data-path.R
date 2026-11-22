# The data path, saved projects and the methods draft: the confirmation log,
# restores and reversals, the study bundle, replay patches and revisions,
# the methods draft, the session script, reading files, saved projects and
# the session inputs.
#
# Author: Ken Kelley

skip_if_not_installed("shiny")

rd_iso <- shiny::isolate

# A small study: an identifier and eight seven-point items on two
# constructs, with the respondents' values drawn once with a fixed seed.
rd_frame <- function(n = 60L, seed = 1L) {
  withr::with_seed(seed, {
    f1 <- stats::rnorm(n)
    f2 <- stats::rnorm(n)
    one <- function(f) pmin(7L, pmax(1L, as.integer(round(4 + 1.4 * f + stats::rnorm(n)))))
    d <- data.frame(id = seq_len(n) + 1000L)
    for (j in 1:4) d[[paste0("a", j)]] <- one(f1)
    for (j in 1:4) d[[paste0("b", j)]] <- one(f2)
    d
  })
}

rd_state <- function(d = rd_frame(), name = "rd.csv") {
  st <- sieve_new_state()
  out <- sieve_load_frame(st, d, name, sprintf("d <- read.csv(\"%s\")", name))
  expect_true(out$ok)
  st
}

rd_log <- function(st) paste(rd_iso(st$codelog), collapse = "\n")

# ---- the confirmation log counts the included items --------------------------

test_that("the confirmation log counts the types of the included items only", {
  st <- rd_state()
  rd_iso({
    m <- st$meta
    m$status[m$item %in% c("b3", "b4")] <- "parked"
    m$type[m$item %in% c("b3", "b4")] <- "ordered"
    st$meta <- m
  })
  sieve_confirm_types(st, "button")
  log <- rd_log(st)
  expect_match(log, "Measurement levels confirmed for 6 active items: 6 continuous, 0 ordered, 0 binary.", fixed = TRUE)
  expect_false(grepl("6 active items: 8 continuous", log, fixed = TRUE))
})

# ---- a restore after a reversal stays on the scale -----------------------------

# q1 on 2 to 7 except one respondent (row 17) who chose 1.
rd_one_low_response <- function() {
  withr::with_seed(1, {
    n <- 60
    d <- data.frame(
      id = seq_len(n), q1 = sample(2:7, n, TRUE), q2 = sample(1:7, n, TRUE),
      q3 = sample(1:7, n, TRUE), q4 = sample(1:7, n, TRUE)
    )
  })
  d$q1[17] <- 1L
  d
}

test_that("observed bounds come from the data as loaded, so a restored respondent stays on the scale", {
  d <- rd_one_low_response()
  st <- rd_state(d, "one_low_response.csv")
  sieve_confirm_types(st, "button")
  invisible(sieve_remove_cases(st, 17L, "flagged"))
  r <- sieve_apply_reverse(st, "q1")
  expect_identical(c(r$applied$q1$lo, r$applied$q1$hi), c(1, 7))
  expect_identical(r$applied$q1$bounds, "observed")
  out <- sieve_restore_cases(st, 17L)
  expect_identical(out$restored, 17L)
  it <- rd_iso(st$items)
  m <- rd_iso(st$meta)
  expect_identical(as.numeric(it$q1[rd_iso(sieve_case_index(st)) == 17L]), 7)
  expect_identical(as.numeric(it$q1), 8 - as.numeric(d$q1))
  expect_length(sieve_outside_bounds(it$q1, m$rev_lo[m$item == "q1"], m$rev_hi[m$item == "q1"]), 0L)
  expect_identical(c(m$min[m$item == "q1"], m$max[m$item == "q1"]), c(1, 7))
})

test_that("a restore whose held value lies outside the bounds of a later reflection is refused", {
  d <- rd_one_low_response()
  d$q2[23] <- 9L
  st <- rd_state(d, "one_low_response2.csv")
  sieve_confirm_types(st, "button")
  refused <- sieve_apply_reverse(st, "q2", lo = 1, hi = 7)
  expect_length(refused$applied, 0L)
  invisible(sieve_remove_cases(st, 23L, "code 9 on q2"))
  ok <- sieve_apply_reverse(st, "q2", lo = 1, hi = 7)
  expect_length(ok$applied, 1L)
  before <- rd_iso(list(items = st$items, raw = st$raw, removed = st$removed_cases, log = st$codelog))
  out <- sieve_restore_cases(st, 23L)
  expect_length(out$restored, 0L)
  expect_match(out$refused, "row 23 holds 9 on q2, outside the bounds 1 to 7", fixed = TRUE)
  after <- rd_iso(list(items = st$items, raw = st$raw, removed = st$removed_cases, log = st$codelog))
  expect_identical(after, before)
  # Through the observation service the Screening tab calls, the refusal is
  # returned and nothing changes.
  via <- sieve_set_case_status(st, 23L, "included")
  expect_match(via$refused, "no respondent was restored", fixed = TRUE)
  expect_identical(rd_iso(st$removed_cases), before$removed)
})

# ---- description-row reverse marks follow the verify rule ----------------------

rd_html <- function(out) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(out$html), collapse = " ")))

# A file in the one-file form: construct and reverse rows (and, when given,
# a reverse_applied row) under the header, then the data rows.
rd_described_csv <- function(d, reverse, applied = NULL) {
  items <- setdiff(names(d), "id")
  cons <- ifelse(startsWith(items, "a"), "A", "B")
  rows <- list(
    c("construct", cons),
    c("reverse", ifelse(items %in% reverse, "yes", ""))
  )
  if (!is.null(applied)) {
    rows[[3]] <- c("reverse_applied", ifelse(items %in% names(applied), ifelse(applied[items], "yes", "no"), ""))
  }
  top <- do.call(rbind, rows)
  colnames(top) <- names(d)
  out <- rbind(as.data.frame(top, stringsAsFactors = FALSE), as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE))
  f <- tempfile("described_", fileext = ".csv")
  utils::write.csv(out, f, row.names = FALSE)
  f
}

rd_load_described <- function(f) {
  st <- sieve_new_state()
  read <- sieve_read_data(f, name = basename(f))
  expect_true(read$ok)
  lf <- sieve_load_frame(st, read$value, basename(f), read$code, description = read$description)
  expect_true(lf$ok)
  list(state = st, load = lf, read = read)
}

test_that("a marked item whose values already correlate positively with its construct is not reversed again", {
  d <- rd_frame()
  # a4 and b4 are reverse keyed, and this prepared copy already holds them
  # reverse scored: their values correlate positively with their constructs.
  f <- rd_described_csv(d, reverse = c("a4", "b4"))
  x <- rd_load_described(f)
  st <- x$state
  expect_setequal(names(x$load$described$held), c("a4", "b4"))
  expect_true(all(x$load$described$held > 0))
  expect_length(x$load$described$reversed, 0L)
  expect_identical(as.numeric(rd_iso(st$items)$a4), as.numeric(d$a4))
  m <- rd_iso(st$meta)
  expect_false(any(m$reverse[m$item %in% c("a4", "b4")]))
  expect_match(sieve_reverse_held_text(x$load$described$held), "already correlate positively with the rest of their construct", fixed = TRUE)
  expect_match(sieve_described_text(m, x$load$described), "2 marked items not reverse scored", fixed = TRUE)
})

test_that("a marked item as administered is reverse scored after the check", {
  d <- rd_frame()
  d$a4 <- 8L - d$a4
  f <- rd_described_csv(d, reverse = "a4")
  x <- rd_load_described(f)
  expect_identical(x$load$described$reversed, "a4")
  expect_true(x$load$described$checked[["a4"]] < 0)
  expect_identical(as.numeric(rd_iso(x$state$items)$a4), 8 - as.numeric(d$a4))
})

test_that("a reverse_applied row settles a marked item without the check", {
  d <- rd_frame()
  f <- rd_described_csv(d, reverse = c("a4", "b4"), applied = c(a4 = TRUE, b4 = FALSE))
  x <- rd_load_described(f)
  st <- x$state
  # The row is a description row, not data.
  expect_identical(nrow(rd_iso(st$raw)), nrow(d))
  expect_true(is.numeric(rd_iso(st$items)$a1))
  expect_identical(x$load$described$recorded, "a4")
  expect_identical(x$load$described$reversed, "b4")
  expect_length(x$load$described$held, 0L)
  m <- rd_iso(st$meta)
  expect_true(m$reverse[m$item == "a4"])
  expect_identical(as.numeric(rd_iso(st$items)$a4), as.numeric(d$a4))
  expect_identical(as.numeric(rd_iso(st$items)$b4), 8 - as.numeric(d$b4))
})

test_that("the Ingest tab asks about held items and reverses them on request", {
  d <- rd_frame()
  f <- rd_described_csv(d, reverse = "a4")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd", five_continuous = TRUE)
    session$setInputs(file = list(name = "described.csv", datapath = f))
    session$setInputs(load = 1L)
    expect_match(rd_html(output$reverse_ask), "Reverse keyed items to settle.", fixed = TRUE)
    expect_false(isTRUE(state$meta$reverse[state$meta$item == "a4"]))
    session$setInputs(reverse_held_go = 1L)
    expect_true(isTRUE(state$meta$reverse[state$meta$item == "a4"]))
    expect_identical(as.numeric(state$items$a4), 8 - as.numeric(d$a4))
    expect_false(grepl("Reverse keyed items to settle", rd_html(output$reverse_ask), fixed = TRUE))
  })
})

test_that("the template of a described file leaves reverse_applied blank where the check finds the reversal already held", {
  d <- rd_frame()
  f <- rd_described_csv(d, reverse = "a4")
  tpl <- sieve_specification_template(f)
  expect_identical(tpl$reverse[tpl$variable == "a4"], "TRUE")
  expect_identical(tpl$reverse_applied[tpl$variable == "a4"], "")
  err <- tryCatch(
    utils::capture.output(sieve_prepare_study(f, tpl, file = tempfile(fileext = ".siever.rds"))),
    error = function(e) e
  )
  expect_s3_class(err, "sieve_import_error")
  expect_match(conditionMessage(err), "does not say whether the data file already holds the reversal", fixed = TRUE)
})

# ---- the study bundle under a clustered design --------------------------------

test_that("the matched study bundle is written for a clustered design, without the cluster column", {
  d <- rd_frame()
  d$school <- rep(c(1L, 2L, 3L), length.out = nrow(d))
  st <- rd_state(d)
  sieve_confirm_types(st, "button")
  rd_iso(st$sampling_design <- list(unit = "clustered", cluster = "school"))
  file <- tempfile("bundle_", fileext = ".tar.gz")
  manifest <- sieve_export_study_bundle(sieve_project_snapshot(st), file, values = "raw")
  expect_identical(manifest$rows, nrow(d))
  dir <- tempfile("bundle_")
  dir.create(dir)
  utils::untar(file, exdir = dir)
  shared <- sieve_project_read(file.path(dir, "project.siever.rds"))
  expect_identical(shared$other$sampling_design$unit, "clustered")
  expect_null(shared$other$sampling_design$cluster)
  expect_false("school" %in% names(shared$raw))
  expect_false("school" %in% names(utils::read.csv(file.path(dir, "data.csv"))))
  # The session's own design is untouched.
  expect_identical(rd_iso(st$sampling_design$cluster), "school")
})

# ---- replay patches carry the values the analysis holds ------------------------

test_that("a replayed reversal of an SPSS labelled item holds the values the analysis holds", {
  skip_if_not_installed("haven")
  d <- rd_frame()
  for (v in paste0("a", 1:4)) {
    d[[v]] <- haven::labelled(d[[v]], labels = c(Disagree = 1, Agree = 7), label = paste("Item", v))
  }
  st <- rd_state(d, "rd.sav")
  sieve_confirm_types(st, "button")
  out <- sieve_apply_reverse(st, "a2", lo = 1, hi = 7)
  expect_length(out$applied, 1L)
  store <- sieve_replay_bundle(st)
  steps <- rd_iso(st$replay_steps)
  key <- steps[[as.character(max(as.integer(names(steps))))]]
  x <- sieve_replay_expand(store$revisions[[key]], store$data)
  expect_identical(x$items$a2, rd_iso(st$items$a2))
  expect_null(attr(x$items$a2, "labels"))
  expect_identical(as.numeric(x$items$a2), 8 - as.numeric(unclass(d$a2)))
  # An unchanged labelled item is still the column as read.
  expect_identical(x$items$a1, rd_iso(st$items$a1))
})

# ---- the revision knows which respondents are in it ----------------------------

test_that("swapping which of two identical respondents is removed changes the revision", {
  d <- rd_frame()
  d[6, -1] <- d[5, -1]
  d$criterion <- seq_len(nrow(d)) / 10
  st <- rd_state(d)
  sieve_confirm_types(st, "button")
  invisible(sieve_remove_cases(st, 5L, "first"))
  r1 <- sieve_revision(st)
  invisible(sieve_restore_cases(st, 5L))
  full <- sieve_revision(st)
  invisible(sieve_remove_cases(st, 6L, "second"))
  r2 <- sieve_revision(st)
  # The item values are the same, the respondents are not.
  expect_identical(r1$data_key, r2$data_key)
  expect_false(sieve_same_revision(r1, r2))
  expect_match(sieve_stale_reason(r1, r2), "respondents were removed", fixed = TRUE)
  # A full restore gives back the revision the session started with.
  invisible(sieve_restore_cases(st, 6L))
  expect_true(sieve_same_revision(full, sieve_revision(st)))
  expect_identical(full, sieve_revision(st))
  # A revision that does not record its respondents compares on the item
  # values.
  old <- r1[c("dataset_id", "data_key", "meta_key")]
  expect_true(sieve_same_revision(old, r2))
})

# ---- the methods draft ---------------------------------------------------------

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")

# The nine tests with 10 percent of each item missing, typed by
# construct and confirmed, with three respondents removed on screening.
rd_missing_study <- function() {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  withr::with_seed(1, for (v in fx$item_cols) d[[v]][sample(nrow(d), 30)] <- NA)
  st <- sieve_new_state()
  invisible(sieve_load_frame(st, d, "hs9_missing.csv", "d <- read.csv(\"hs9_missing.csv\")"))
  m <- rd_iso(st$meta)
  for (cn in names(fx$theory_map)) m$construct[m$item %in% fx$theory_map[[cn]]] <- cn
  rd_iso(st$meta <- m)
  sieve_confirm_types(st, "button")
  invisible(sieve_remove_cases(st, c(3L, 4L), "careless responding"))
  invisible(sieve_remove_cases(st, 5L, ""))
  list(state = st, fx = fx)
}

rd_draft <- function(st, ...) {
  rd_iso(sieve_methods_draft(st$items, st$meta,
    dataset_name = "hs9_missing.csv", revision = sieve_revision(st),
    estimator = sieve_estimator_effective("study", st), removed = sieve_removed_table(st), ...
  ))
}

rd_lines <- function(draft, pattern) {
  lines <- strsplit(draft, "\n", fixed = TRUE)[[1]]
  lines[grepl(pattern, lines)]
}

test_that("the draft counts the removed respondents and states each coefficient's case treatment", {
  pm <- rd_missing_study()
  st <- pm$state
  draft <- rd_draft(st, reliability = list())
  expect_match(draft, paste(
    "Of the 301 respondents in the data as loaded, 3 were removed from the analysis during screening",
    "(2 for careless responding; 1 with no reason recorded), so the analytic sample comprised 298 respondents"
  ), fixed = TRUE)
  # The study is MLR: the CFA and each omega use full information maximum
  # likelihood, and each line says so with the n the coefficient used.
  expect_match(draft, "with full information maximum likelihood, so incomplete cases contribute to the CFA", fixed = TRUE)
  expect_match(draft, "incomplete cases entered through full information maximum likelihood, as in the CFA", fixed = TRUE)
  rel <- rd_lines(draft, "^- (Visual|Textual|Speed):")
  expect_length(rel, 3L)
  for (cn in c("Visual", "Textual", "Speed")) {
    sp <- sieve_reliability_spec(pm$fx$theory_map[[cn]], rep("continuous", 3), construct = cn, missing = "fiml", ci_method = "none")
    r <- sieve_reliability_omega(rd_iso(st$items), sp)
    line <- rel[startsWith(rel, paste0("- ", cn, ":"))]
    expect_match(line, sprintf(
      "%s = %.2f (full information maximum likelihood, n = %d of 298)",
      sieve_draft_coefficient(sp), r$estimate, r$n$used
    ), fixed = TRUE)
  }
  # Green and Yang (2009) is listed only when categorical omega is reported.
  expect_false(grepl("Green, S. B., & Yang", draft, fixed = TRUE))
  # Under ML the CFA and each omega use the complete cases, which is said.
  rd_iso(st$estimator_choice <- "ml")
  ml <- rd_draft(st, reliability = list())
  expect_match(ml, "on the complete cases (listwise deletion, SIEVER's setting for this estimator)", fixed = TRUE)
  its <- pm$fx$theory_map$Speed
  n_cc <- sum(stats::complete.cases(rd_iso(st$items)[, its]))
  expect_match(rd_lines(ml, "^- Speed:"), sprintf("(complete cases on its items (listwise deletion), n = %d of 298)", n_cc), fixed = TRUE)
})

test_that("the draft follows the Reliability tab's item treatment", {
  st <- rd_state()
  rd_iso({
    m <- st$meta
    m$construct <- ifelse(startsWith(m$item, "a"), "A", "B")
    st$meta <- m
  })
  sieve_confirm_types(st, "button")
  draft <- rd_iso(sieve_methods_draft(st$items, st$meta,
    revision = sieve_revision(st), reliability = list(treatment = "ordered")
  ))
  sp <- sieve_reliability_spec(paste0("a", 1:4), rep("ordered", 4), construct = "A", ci_method = "none")
  r <- sieve_reliability_omega(rd_iso(st$items), sp)
  expect_identical(sp$coefficient, "omega_categorical")
  expect_match(rd_lines(draft, "^- A:"), sprintf(
    "- A: Robust omega for ordered items (observed denominator, built from the unrestricted polychoric correlations; the reliability of the sum of category ranks) = %.2f (complete cases on its items (listwise deletion), n = 60 of 60)",
    r$estimate
  ), fixed = TRUE)
  # Categorical omega's interval is a bootstrap run on the Reliability tab.
  expect_match(rd_lines(draft, "^- A:"), "interval: bootstrap not run yet (", fixed = TRUE)
  expect_match(draft, "with the items treated as ordered (a binary item as binary) (the Reliability tab's item treatment)", fixed = TRUE)
  expect_match(draft, "full information maximum likelihood, used in the CFA, is not offered for ordered or binary items", fixed = TRUE)
  # Robust omega is cited to Kelley and Pornprasertmanit (2016); Green and
  # Yang (2009) only for the model-implied denominator of ordered items.
  expect_match(draft, "built from the unrestricted polychoric correlations; Kelley & Pornprasertmanit, 2016", fixed = TRUE)
  expect_false(grepl("Green, S. B., & Yang, Y. (2009)", draft, fixed = TRUE))
})

test_that("under a WLSMV study the draft reports the Reliability tab's categorical omega", {
  st <- rd_state()
  rd_iso({
    m <- st$meta
    m$construct <- ifelse(startsWith(m$item, "a"), "A", "B")
    st$meta <- m
    st$estimator_choice <- "wlsmv"
  })
  sieve_confirm_types(st, "button")
  draft <- rd_iso(sieve_methods_draft(st$items, st$meta,
    estimator = "wlsmv", revision = sieve_revision(st), reliability = list()
  ))
  sp <- sieve_reliability_spec(paste0("a", 1:4), rep("ordered", 4), construct = "A", ci_method = "none")
  r <- sieve_reliability_omega(rd_iso(st$items), sp)
  expect_match(draft, sprintf("- A: Robust omega for ordered items (observed denominator, built from the unrestricted polychoric correlations; the reliability of the sum of category ranks) = %.2f", r$estimate), fixed = TRUE)
  expect_match(draft, "SIEVER's setting for the categorical estimator; lavaan also offers pairwise deletion", fixed = TRUE)
  expect_false(grepl("as the categorical estimator requires", draft, fixed = TRUE))
})

test_that("a saved bootstrap of the same coefficient on the same data is found, and only then", {
  pm <- rd_missing_study()
  st <- pm$state
  its <- pm$fx$theory_map$Visual
  run <- sieve_reliability_spec(its, rep("continuous", 3), construct = "Visual",
    missing = "fiml", ci_method = "percentile", denominator = "observed",
    B = 400L, seed = 7L, run_bootstrap = TRUE, group_id = "Visual"
  )
  om <- sieve_reliability_omega(rd_iso(st$items), run)
  expect_identical(om$uncertainty$status, "completed")
  sieve_register_reliability(st, list(Visual = list(spec = run, omega = om, alpha = NULL)),
    revision = sieve_revision(st)
  )
  wanted <- sieve_reliability_spec(its, rep("continuous", 3), construct = "Visual",
    missing = "fiml", ci_method = "percentile", denominator = "observed", run_bootstrap = FALSE
  )
  hit <- sieve_draft_bootstrap(rd_iso(st$analyses), wanted, sieve_revision(st))
  expect_identical(hit$id, "A1")
  expect_identical(c(hit$omega$lower, hit$omega$upper), c(om$lower, om$upper))
  # Not for another coefficient, another case treatment, or other respondents.
  other <- sieve_reliability_spec(its, rep("continuous", 3), construct = "Visual",
    missing = "listwise", ci_method = "percentile", denominator = "observed", run_bootstrap = FALSE
  )
  expect_null(sieve_draft_bootstrap(rd_iso(st$analyses), other, sieve_revision(st)))
  invisible(sieve_restore_cases(st, 5L))
  expect_null(sieve_draft_bootstrap(rd_iso(st$analyses), wanted, sieve_revision(st)))
})

test_that("with the default reliability target the draft reports robust omega, its bootstrap status, and model implied omega beside it", {
  skip_if_not("recommended" %in% eval(formals(sieve_reliability_spec)$ci_method),
    "the Reliability functions do not accept ci_method = \"recommended\"")
  pm <- rd_missing_study()
  st <- pm$state
  draft <- rd_draft(st, reliability = list(), analyses = list())
  line <- rd_lines(draft, "^- Visual:")
  expect_match(line, "- Visual: Robust omega (observed denominator) = ", fixed = TRUE)
  expect_match(line, "(full information maximum likelihood, n = ", fixed = TRUE)
  expect_match(line, "interval: bootstrap not run yet (Bootstrap percentile; run it on the Reliability tab, then refresh the draft)", fixed = TRUE)
  expect_match(line, "Beside it, Omega (model-implied denominator) = ", fixed = TRUE)
  expect_match(line, "MLR (sandwich) SE, logit-scale limits (delta method)", fixed = TRUE)
  # A saved bootstrap of robust omega on these data supplies the interval.
  its <- pm$fx$theory_map$Visual
  run <- sieve_reliability_spec(its, rep("continuous", 3), construct = "Visual",
    missing = "fiml", ci_method = "percentile", denominator = "observed",
    B = 400L, seed = 7L, run_bootstrap = TRUE, group_id = "Visual"
  )
  om <- sieve_reliability_omega(rd_iso(st$items), run)
  expect_identical(om$uncertainty$status, "completed")
  sieve_register_reliability(st, list(Visual = list(spec = run, omega = om, alpha = NULL)),
    revision = sieve_revision(st)
  )
  ran <- rd_draft(st, reliability = list(), analyses = rd_iso(st$analyses))
  expect_match(rd_lines(ran, "^- Visual:"), sprintf(
    "95%% CI [%.2f, %.2f], %s (", om$lower, om$upper, om$uncertainty$label
  ), fixed = TRUE)
  expect_match(rd_lines(ran, "^- Visual:"), "from the bootstrap run on the Reliability tab on these data (analysis A1)", fixed = TRUE)
})

# ---- the session script carries no respondent identifier -----------------------

test_that("the session script names removed respondents by row, never by identifier, and the panel says reasons are carried", {
  d <- rd_frame()
  d$id <- sprintf("R_%04dXq", seq_len(nrow(d)))
  st <- rd_state(d)
  sieve_confirm_types(st, "button")
  invisible(sieve_remove_cases(st, c(4L, 9L), "flagged long strings"))
  invisible(sieve_restore_cases(st, 9L))
  script <- rd_iso(sieve_replay_script(st, data_file = "siever_inputs.rds"))
  expect_false(grepl("R_0004Xq", script, fixed = TRUE))
  expect_false(grepl("R_0009Xq", script, fixed = TRUE))
  expect_match(script, "rows 4, 9 of the data as loaded", fixed = TRUE)
  expect_match(script, "out <- case_index %in% c(4L, 9L)", fixed = TRUE)
  expect_match(script, "flagged long strings", fixed = TRUE)
  ui <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(mod_repro_ui("repro")), collapse = "")))
  expect_match(ui, "carries the reason typed for each removal as a comment", fixed = TRUE)
  # The replayed removal gives the same rows.
  env <- new.env(parent = globalenv())
  eval(parse(text = rd_iso(sieve_replay_script(st))), envir = env)
  expect_identical(as.integer(env$case_index), rd_iso(sieve_case_index(st)))
})

# ---- a decimal comma with description rows --------------------------------------

test_that("a semicolon file with a decimal comma and description rows reads its data columns as numbers", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("id;a;b;c", "construct;X;X;X", "1;3,5;2,25;4,75", "2;1,5;3,25;2,75", "3;2,5;4,5;3,5", "4;4,5;1,75;4,25"), f)
  r <- sieve_read_delimited(f, "eu.csv", sep = ";", dec = ",", description = TRUE)
  expect_true(r$ok)
  expect_identical(r$description$construct, c("X", "X", "X"))
  expect_true(all(vapply(r$value[c("a", "b", "c")], is.numeric, logical(1))))
  expect_identical(r$value$b, c(2.25, 3.25, 4.5, 1.75))
  # The recorded R reads the same data frame.
  env <- new.env()
  withr::with_dir(dirname(f), eval(parse(text = sub("eu.csv", basename(f), r$code, fixed = TRUE)), envir = env))
  expect_identical(env$d, r$value)
})

# ---- the read step writes the file name as a literal ---------------------------

test_that("the recorded read step writes the file name as an R string literal", {
  dir <- tempfile("named_")
  dir.create(dir)
  name <- "survey \"wave 2\" C:\\data.csv"
  d <- rd_frame(n = 10L)
  utils::write.csv(d, file.path(dir, name), row.names = FALSE)
  read <- sieve_read_data(file.path(dir, name), name = name)
  expect_true(read$ok)
  expr <- parse(text = read$code)
  env <- new.env()
  withr::with_dir(dir, eval(expr, envir = env))
  expect_identical(env$d, read$value)
})

# ---- the undo message after a removal --------------------------------------------

test_that("the live Reverse? action restores original responses after respondent removal", {
  d <- rd_frame()
  st <- rd_state(d)
  sieve_confirm_types(st, "button")
  # Item data that are not the values as loaded, so the reversal is of the
  # current values.
  rd_iso({
    it <- st$items
    it$a1 <- pmin(7L, it$a1 + 0L)
    raw <- st$raw
    raw$a1 <- 8L - raw$a1
    st$raw <- raw
    st$items <- it
  })
  out <- sieve_apply_reverse(st, "a1", lo = 1, hi = 7)
  rec <- out$applied$a1
  expect_true(is.na(rec$applied))
  invisible(sieve_remove_cases(st, 3L, "flagged"))
  # The Reverse? cell takes it back exactly.
  back <- sieve_apply_reverse(st, "a1")
  expect_length(back$applied, 1L)
  keep <- setdiff(seq_len(nrow(d)), 3L)
  expect_identical(as.numeric(rd_iso(st$items)$a1), as.numeric(d$a1[keep]))
})

# ---- Where each type came from, and the five-option rule for saved projects ------

test_that("every path that sets a type records where it came from (type_source)", {
  st <- rd_state()
  m <- rd_iso(st$meta)
  expect_identical(unique(sieve_type_source(m)), "proposed")
  expect_identical(names(m)[match("type", names(m)) + 1L], "type_source")
  # The Type cell (a metadata patch) is the researcher's choice; its undo
  # gives back where the type came from.
  sieve_apply_metadata_patch(st, sieve_metadata_patch(rd_iso(st$meta), "a1", "type", "ordered"), "Set item type")
  expect_identical(sieve_type_source(rd_iso(st$meta))[1:2], c("chosen", "proposed"))
  sieve_undo_metadata_edit(st)
  expect_identical(rd_iso(st$meta$type[1]), "continuous")
  expect_identical(sieve_type_source(rd_iso(st$meta))[1], "proposed")
  # Set the type of, and the bulk button (which also switches a chosen type).
  sieve_set_types(st, c("a2", "a3"), "ordered", "two items")
  m <- rd_iso(st$meta)
  expect_identical(sieve_type_source(m)[m$item %in% c("a2", "a3")], c("chosen", "chosen"))
  out <- sieve_bulk_type(st, "continuous")
  expect_setequal(out$changed, c("a2", "a3"))
  m <- rd_iso(st$meta)
  expect_identical(m$type[m$item %in% c("a2", "a3")], c("continuous", "continuous"))
  expect_identical(sieve_type_source(m)[m$item %in% c("a2", "a3")], c("chosen", "chosen"))
  # The specification carries the column.
  spec <- rd_iso(sieve_spec_table(st$raw, st$items, st$meta, st$roles))
  src <- stats::setNames(spec$type_source, spec$variable)
  expect_identical(unname(src[c("id", "a1", "a2")]), c("", "proposed", "chosen"))
})

test_that("a specification's type_source is read: a chosen ordered type is kept by the five-option rule, a proposed one switches", {
  st <- rd_state()
  spec <- rd_iso(sieve_spec_table(st$raw, st$items, st$meta, st$roles))
  spec$type[spec$variable %in% c("b1", "b2")] <- "ordered"
  spec$type_source[spec$variable == "b1"] <- "chosen"
  spec$type_source[spec$variable == "b2"] <- ""
  out <- sieve_apply_spec_table(st, spec, min_options = 5L)
  expect_true(out$ok)
  m <- rd_iso(st$meta)
  expect_identical(m$type[m$item %in% c("b1", "b2")], c("ordered", "continuous"))
  expect_identical(sieve_type_source(m)[m$item %in% c("b1", "b2")], c("chosen", "proposed"))
  expect_identical(out$summary$kept_chosen, "b1")
  expect_match(out$message, "Kept ordered because the specification records the type as chosen: b1.", fixed = TRUE)
  expect_match(out$message, "performs comparably to the categorical estimator in many conditions", fixed = TRUE)
  # A specification without the column reads as proposed.
  st2 <- rd_state()
  old <- spec
  old$type_source <- NULL
  out2 <- sieve_apply_spec_table(st2, old, min_options = 5L)
  m2 <- rd_iso(st2$meta)
  expect_identical(m2$type[m2$item %in% c("b1", "b2")], c("continuous", "continuous"))
  # A cell outside the vocabulary is refused with the others.
  bad <- spec
  bad$type_source[bad$variable == "b1"] <- "mine"
  refused <- sieve_apply_spec_table(rd_state(), bad, min_options = 5L)
  expect_false(refused$ok)
  expect_match(paste(refused$problems, collapse = " "), "A type_source cell is neither proposed nor chosen", fixed = TRUE)
})

test_that("the project bar's Open follows the five-option rule with the Ingest checkbox, and says so before opening", {
  original <- rd_state()
  rd_iso({
    m <- original$meta
    m$type[m$item %in% c("a1", "a2")] <- "ordered"
    m$type_source[m$item == "a2"] <- "chosen"
    original$meta <- m
  })
  sieve_confirm_types(original, "button")
  file <- tempfile(fileext = ".siever.rds")
  sieve_project_write(sieve_project_snapshot(original), file)
  st <- sieve_new_state()
  shiny::testServer(mod_project_server, args = list(state = st), {
    session$setInputs(open = 1L)
    session$setInputs(file = list(name = "study.siever.rds", datapath = file))
    preview <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", output$open_preview$html))
    expect_match(preview, "1 item (a1) typed ordered with five or more response options will open as continuous", fixed = TRUE)
    expect_match(preview, "Kept ordered as your choice: a2.", fixed = TRUE)
    session$setInputs(restore = 1L)
    m <- state$meta
    expect_identical(m$type[m$item %in% c("a1", "a2")], c("continuous", "ordered"))
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$project$switched_types$items, "a1")
    expect_match(state$project$switched_types$text, "1 item (a1) typed ordered in the project has five or more response options", fixed = TRUE)
    # The saved copy is the file as it was, so the switch is a change from it.
    expect_false(identical(state$project_control$acknowledged, sieve_project_fingerprint(state)))
  })
  # With the Ingest checkbox off the project bar keeps the saved types.
  st_off <- sieve_new_state()
  shiny::testServer(mod_project_server, args = list(state = st_off), {
    state$five_continuous <- FALSE
    session$setInputs(open = 1L)
    session$setInputs(file = list(name = "study.siever.rds", datapath = file))
    session$setInputs(restore = 1L)
    expect_identical(state$meta$type[state$meta$item == "a1"], "ordered")
    expect_null(state$project$switched_types)
  })
  # Keeping them ordered records them as chosen, so a project saved again
  # opens with them ordered.
  sieve_keep_ordered(st)
  m <- rd_iso(st$meta)
  expect_identical(m$type[m$item == "a1"], "ordered")
  expect_identical(sieve_type_source(m)[m$item == "a1"], "chosen")
  again <- tempfile(fileext = ".siever.rds")
  sieve_project_write(sieve_project_snapshot(st), again)
  st3 <- sieve_new_state()
  opened <- sieve_project_open(st3, sieve_project_read(again), source = "again.siever.rds")
  expect_length(opened$rule$switched, 0L)
  expect_identical(rd_iso(st3$meta$type[st3$meta$item == "a1"]), "ordered")
})

test_that("the Type cell says whether a type is proposed or chosen, with the item's options and skew", {
  st <- rd_state()
  m <- rd_iso(st$meta)
  it <- rd_iso(st$items)
  note <- sieve_type_cell_note(m, match("a1", m$item), it)
  ev <- sieve_type_evidence(it$a1)
  expect_match(note, sprintf("proposed; %s", ev$text), fixed = TRUE)
  expect_match(note, "title=\"Category counts: 1: ", fixed = TRUE)
  x <- it$a1
  sk <- mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5
  expect_equal(ev$skew, sk)
  expect_identical(sum(ev$counts), length(x))
  # The rule's sentence states the finding conditionally.
  txt <- sieve_option_rule_text("q1", "the project")
  expect_match(txt, "performs comparably to the categorical estimator in many conditions, especially with roughly symmetric thresholds", fixed = TRUE)
  expect_match(txt, "with fewer categories or strongly asymmetric thresholds the categorical estimator is more accurate", fixed = TRUE)
  expect_false(grepl("loses little", txt, fixed = TRUE))
})

test_that("the draft labels each coefficient by its denominator and says what robust means (DMAR's definition)", {
  pm <- rd_missing_study()
  st <- pm$state
  draft <- rd_draft(st, reliability = list())
  rel <- rd_lines(draft, "^- (Visual|Textual|Speed):")
  # No value is labeled with an unqualified omega.
  expect_false(any(grepl("(^|[:,] )omega = ", rel)))
  expect_true(all(grepl("(Robust omega \\(observed denominator\\)|Omega \\(model-implied denominator\\)) = ", rel)))
  if (any(grepl("Robust omega (observed denominator) = ", rel, fixed = TRUE))) {
    expect_match(draft, "robust to misspecification of the composite's total variance only", fixed = TRUE)
    expect_match(draft, "Kelley and Pornprasertmanit's (2016) hierarchical omega (not the bifactor omega hierarchical)", fixed = TRUE)
  }
})

# ---- SPSS value labels declare the response options -----------------------------

test_that("SPSS value labels give an item its declared response options and bounds, user-missing codes left out", {
  skip_if_not_installed("haven")
  n <- 120L
  lab7 <- c("Strongly disagree" = 1, "Neutral" = 4, "Strongly agree" = 7, "Refused" = 99)
  mk <- function(v) haven::labelled_spss(as.numeric(v), labels = lab7, na_values = 99)
  d <- withr::with_seed(1, {
    data.frame(
      id = seq_len(n),
      q1 = mk(sample(2:5, n, TRUE)),
      q2 = mk(sample(1:7, n, TRUE)),
      q3 = mk(sample(c(1:7, 99), n, TRUE)),
      q7 = mk(sample(c(1, 7), n, TRUE))
    )
  })
  f <- tempfile(fileext = ".sav")
  haven::write_sav(d, f)
  read <- sieve_read_data(f, name = "labelled.sav")
  expect_true(read$ok)
  st <- sieve_new_state()
  expect_true(sieve_load_frame(st, read$value, "labelled.sav", read$code)$ok)
  m <- rd_iso(st$meta)
  row <- function(it) m[m$item == it, , drop = FALSE]
  # Four of seven labelled points observed: seven options, so continuous by
  # the five-option rule, with the labels' bounds (not 99) declared.
  expect_identical(row("q1")$type, "continuous")
  expect_identical(c(row("q1")$rev_lo, row("q1")$rev_hi), c(1, 7))
  expect_identical(row("q1")$rev_bounds, "declared")
  expect_identical(c(row("q3")$rev_lo, row("q3")$rev_hi), c(1, 7))
  # Two observed values stay binary, with the declared bounds kept for a
  # reversal.
  expect_identical(row("q7")$type, "binary")
  expect_identical(c(row("q7")$rev_lo, row("q7")$rev_hi), c(1, 7))
  tab <- sieve_item_options(rd_iso(st$items), m)
  expect_identical(tab$options[tab$item == "q1"], 7L)
  expect_match(paste(rd_iso(st$codelog), collapse = "\n"), "#   q1: 1 to 7", fixed = TRUE)
  # A reversal of q1 reflects within the scale, not within 2 to 5.
  out <- sieve_apply_reverse(st, "q1")
  expect_identical(c(out$applied$q1$lo, out$applied$q1$hi), c(1, 7))
})

# ---- the SPSS note gives each item's own reason ----------------------------------

test_that("the SPSS note says five or more response options only of the items the rule typed", {
  meta <- data.frame(
    item = c("q1", "q2", "q3"), label = c("q1", "q2", "q3"),
    type = "continuous", type_source = c("proposed", "chosen", "proposed"),
    status = "active", spss_measure = "Ordinal", stringsAsFactors = FALSE
  )
  items <- data.frame(q1 = rep(1:7, 3), q2 = rep(c(1, 2, 3), 7), q3 = rep(c(1.5, 2.25, 3), 7))
  clash <- sieve_spss_level_conflicts(meta, items)
  expect_identical(clash$reason, c("options", "chosen", "values"))
  expect_identical(clash$options, c(7L, 3L, 3L))
  text <- sieve_spss_reason_text(clash)
  expect_match(text, "SIEVER proposes q1 as continuous because each has five or more response options", fixed = TRUE)
  expect_match(text, "q2 were typed continuous by you.", fixed = TRUE)
  expect_match(text, "q3 hold values that are not whole numbers", fixed = TRUE)
  expect_false(grepl("q2 as continuous because", text, fixed = TRUE))
})

# ---- rows keep their numbers; the preview counts them ----------------------------

test_that("exports after removals carry each row's number in the data as loaded, and the Open preview counts loaded, removed and analytic rows", {
  d <- rd_frame()
  st <- rd_state(d)
  sieve_confirm_types(st, "button")
  # Nothing removed: the export has no row-number column.
  plain <- rd_iso(sieve_export_data(st$raw, st$items, st$meta, rows = st$case_index))
  expect_false("original_row" %in% names(plain))
  invisible(sieve_remove_cases(st, c(2L, 30L), "flagged"))
  out <- rd_iso(sieve_export_data(st$raw, st$items, st$meta, values = "processed", rows = st$case_index))
  expect_identical(names(out)[1], "original_row")
  expect_match(attr(out$original_row, "label"), "Row of the data as loaded", fixed = TRUE)
  out$original_row <- as.integer(unclass(out$original_row))
  attr(out$original_row, "label") <- NULL
  expect_identical(out$original_row, setdiff(seq_len(nrow(d)), c(2L, 30L)))
  expect_identical(out$id, d$id[out$original_row])
  # The matched study bundle keeps them, in data.csv and in its project.
  file <- tempfile(fileext = ".tar.gz")
  invisible(sieve_export_study_bundle(sieve_project_snapshot(st), file))
  dir <- tempfile("bundle_")
  dir.create(dir)
  utils::untar(file, exdir = dir)
  shared_data <- utils::read.csv(file.path(dir, "data.csv"))
  expect_identical(shared_data$original_row, out$original_row)
  shared <- sieve_project_read(file.path(dir, "project.siever.rds"))
  expect_identical(as.integer(shared$other$case_index), out$original_row)
  # The Open preview counts the rows.
  about <- sieve_project_describe(sieve_project_snapshot(st))
  expect_identical(unname(about[["rows"]]), "58 in the analysis (60 loaded, 2 removed on the Screening tab)")
})

# ---- project save and open, and the session inputs ------------------------------

test_that("the plain-data check finds what is not data, even in a column's attribute, and keeps plain columns plain", {
  d <- rd_frame()
  attr(d$a1, "labels") <- c(low = 1, high = 7)
  expect_identical(sieve_not_plain(list(data = d, n = 1:3)), character(0))
  bad <- d
  attr(bad$a2, "hook") <- function() 1
  expect_identical(sieve_not_plain(list(data = bad)), "attr(snapshot$data$a2, \"hook\")")
  expect_identical(sieve_not_plain(list(a = list(b = new.env()))), "snapshot$a$b")
  expect_identical(sieve_not_plain(list(q = quote(x + 1))), "snapshot$q")
})

test_that("a snapshot read and checked is not checked again when it is restored unchanged", {
  st <- rd_state()
  sieve_confirm_types(st, "button")
  f <- tempfile(fileext = ".siever.rds")
  sieve_project_write(sieve_project_snapshot(st), f)
  snap <- sieve_project_read(f)
  checks <- 0L
  testthat::local_mocked_bindings(
    sieve_project_check = function(snapshot) {
      checks <<- checks + 1L
      character(0)
    }
  )
  sieve_project_restore(sieve_new_state(), snap)
  expect_identical(checks, 0L)
  # A snapshot changed after it was read is checked.
  changed <- snap
  changed$meta$label[1] <- "changed"
  sieve_project_restore(sieve_new_state(), changed)
  expect_identical(checks, 1L)
})

test_that("removing respondents stores no new copy of a reversed column, and the session inputs are compressed and read back", {
  st <- rd_state()
  sieve_confirm_types(st, "button")
  invisible(sieve_apply_reverse(st, c("a2", "b3"), lo = 1, hi = 7))
  before <- length(rd_iso(st$replay_data))
  for (r in c(5L, 12L, 40L)) invisible(sieve_remove_cases(st, r, "flagged"))
  # The source data with the removed rows are the same object; the two
  # reversed columns are not stored again for each removal.
  expect_identical(length(rd_iso(st$replay_data)), before)
  dir <- tempfile("session_")
  paths <- sieve_session_write(st, dir)
  bytes <- readBin(paths[["inputs"]], "raw", file.size(paths[["inputs"]]))
  expect_identical(bytes[1], as.raw(0x78))
  expect_lt(length(bytes), length(serialize(rd_iso(sieve_replay_bundle(st)), NULL, version = 3L)) / 2)
  script <- readLines(paths[["script"]])
  hash <- regmatches(script, regexpr("[0-9a-f]{32}", script))[1]
  store <- sieve_replay_read(paths[["inputs"]], hash)
  expect_identical(store$format, "SIEVER replay inputs")
  # An input file written uncompressed is read.
  plain <- tempfile(fileext = ".rds")
  writeBin(serialize(store, NULL, version = 3L), plain)
  expect_identical(sieve_replay_read(plain, sieve_bytes_digest(readBin(plain, "raw", file.size(plain)))), store)
  # The replayed session gives the session's item data.
  env <- new.env(parent = globalenv())
  withr::with_dir(dir, sys.source(paths[["script"]], envir = env))
  expect_identical(sieve_data_key(env$items), rd_iso(sieve_data_key(st$items)))
})

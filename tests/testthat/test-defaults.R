# The defaults: an item with five or more scale points is continuous; the
# estimator is resolved with its reason and is a choice on the CFA tab; a
# question row under the header is the item wording and the reading of
# description rows can be turned off; an item whose observed range differs
# from the study's common range is named, and the bounds of every item can
# be declared at once; the summary export holds the polychoric matrix when
# it is the analysis basis; the role antecedent is offered as a predictor
# on the SEM tab and nowhere as a criterion.
#
# The module tests drive the real observers with shiny::testServer; the
# plain functions are checked against values written out by hand or against
# a direct lavaan fit.

skip_if_not_installed("DMAR")
skip_if_not_installed("shiny")
skip_if_not_installed("lavaan")
withr::local_options(sieve.cores = 2L)

df_strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = " ")))
df_html <- function(out) df_strip(if (is.list(out) && !is.null(out$html)) out$html else out)

df_path <- function(name) {
  path <- file.path(tempfile("df_"), name)
  dir.create(dirname(path))
  path
}

# A .csv whose header is followed by description rows (the words in `rows`,
# in that order) and then the data rows, as text.
df_described_csv <- function(d, rows, name = "described.csv") {
  path <- df_path(name)
  text <- as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
  top <- do.call(rbind, lapply(names(rows), function(word) {
    values <- rows[[word]]
    cells <- vapply(names(d)[-1], function(v) {
      x <- values[v]
      if (is.null(x) || is.na(x)) "" else as.character(x)
    }, character(1))
    as.data.frame(as.list(stats::setNames(c(word, cells), names(d))), stringsAsFactors = FALSE)
  }))
  utils::write.csv(rbind(top, text), path, row.names = FALSE, na = "")
  path
}

# The bundled nine tests, loaded and confirmed, around a fresh state.
df_nine_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$estimator <- "ML with FIML"

  })
  s
}

# Six items with three categories each, from two correlated factors.
df_ordered_frame <- function(n = 240L) {
  withr::with_seed(20261015, {
    f1 <- stats::rnorm(n)
    f2 <- 0.4 * f1 + sqrt(1 - 0.16) * stats::rnorm(n)
    it <- function(f, l) as.integer(cut(l * f + sqrt(1 - l^2) * stats::rnorm(n), c(-Inf, -0.6, 0.6, Inf)))
    data.frame(
      a1 = it(f1, .8), a2 = it(f1, .75), a3 = it(f1, .7),
      b1 = it(f2, .8), b2 = it(f2, .75), b3 = it(f2, .7)
    )
  })
}

# ---- 1. The type proposal ---------------------------------------------------

test_that("an item with five or more distinct integer values is proposed as continuous, three or four as ordered, two as binary", {
  values <- function(k) rep(seq_len(k), length.out = 60L)
  expect_identical(sieve_detect_type(values(2L)), "binary")
  expect_identical(sieve_detect_type(values(3L)), "ordered")
  expect_identical(sieve_detect_type(values(4L)), "ordered")
  expect_identical(sieve_detect_type(values(5L)), "continuous")
  expect_identical(sieve_detect_type(values(7L)), "continuous")
  expect_identical(sieve_detect_type(values(10L)), "continuous")
  # Non integer values are continuous whatever their count; a constant and
  # text are labeled as such.
  expect_identical(sieve_detect_type(c(1.5, 2.5, 3.5, 1.5)), "continuous")
  expect_identical(sieve_detect_type(rep(3, 10)), "constant")
  expect_identical(sieve_detect_type(letters[1:5]), "text")
  # The cutoff is an argument: a rule of ten is one call away.
  expect_identical(sieve_detect_type(values(7L), max_ordered = 10L), "ordered")
  # Through the ledger: a seven point item and a four point item side by side.
  d <- data.frame(seven = values(7L), four = values(4L), two = values(2L))
  m <- sieve_build_meta(d, names(d))
  expect_identical(stats::setNames(m$type, m$item), c(seven = "continuous", four = "ordered", two = "binary"))
  expect_identical(m$detected, m$type)
})

test_that("the estimator is resolved from the types with its reason, without a small sample recommendation to delete cases", {
  cont <- sieve_estimator_resolve(rep("continuous", 9L), n = 301L)
  # Follow the item types gives continuous items DMAR's default, ML with FIML
  # (its fit against lavaan called directly is in
  # test-estimator-default.R).
  expect_identical(cont$label, "ML with FIML")
  expect_identical(cont$basis, "Pearson")
  expect_match(cont$reason, "Every included item is continuous (five or more scale points", fixed = TRUE)
  expect_match(cont$reason, "(n = 301)", fixed = TRUE)
  expect_false(grepl("small", cont$reason, fixed = TRUE))
  small <- sieve_estimator_resolve(rep("continuous", 9L), n = 80L)
  expect_false(grepl("sample is small|alternative in the Estimator control", small$reason))
  expect_identical(small$missing, "fiml")
  expect_true(small$small_n)
  # Exactly 100 is small; 101 is not.
  expect_true(sieve_estimator_resolve(rep("continuous", 3L), n = 100L)$small_n)
  expect_false(sieve_estimator_resolve(rep("continuous", 3L), n = 101L)$small_n)
  cat_ <- sieve_estimator_resolve(c("ordered", "ordered", "continuous"), n = 300L)
  expect_identical(cat_$label, "WLSMV (DWLS)")
  expect_identical(cat_$basis, "mixed (polychoric and Pearson)")
  expect_match(cat_$reason, "2 of the 3 included items are ordered or binary, so WLSMV", fixed = TRUE)
  expect_match(cat_$reason, "lavaan has no FIML for this estimator", fixed = TRUE)
  expect_match(cat_$reason, "Rhemtulla, Brosseau-Liard & Savalei, 2012", fixed = TRUE)
  expect_identical(sieve_estimator_resolve(rep("binary", 4L))$basis, "tetrachoric")
  expect_identical(sieve_estimator_resolve(character(0))$reason, "No items classified yet.")

  # The resolution itself: the choices override the types.
  types <- c("ordered", "continuous", NA)
  auto <- sieve_estimator_resolve(types, choice = "auto")
  expect_identical(auto$estimator, "WLSMV")
  expect_identical(auto$missing, "listwise")
  expect_identical(auto$categorical, c(TRUE, FALSE, FALSE))
  mlr <- sieve_estimator_resolve(types, n = 50L, choice = "mlr")
  expect_identical(mlr$estimator, "MLR")
  expect_identical(mlr$missing, "fiml")
  expect_false(any(mlr$categorical))
  expect_match(mlr$reason, "MLR with FIML was chosen with the Estimator control", fixed = TRUE)
  expect_match(mlr$reason,
    "the 1 item typed ordered or binary included, whose loadings are then attenuated",
    fixed = TRUE
  )
  expect_false(grepl("sample is small", mlr$reason, fixed = TRUE))
  ml <- sieve_estimator_resolve(types, choice = "ml")
  expect_identical(ml$estimator, "ML")
  expect_identical(ml$missing, "listwise")
  expect_match(ml$reason, "listwise deletion", fixed = TRUE)
  wlsmv <- sieve_estimator_resolve(types, choice = "wlsmv")
  expect_identical(wlsmv$estimator, "WLSMV")
  # Every typed item is ordered under the choice; a column with no type never is.
  expect_identical(wlsmv$categorical, c(TRUE, TRUE, FALSE))
  expect_match(wlsmv$reason, "1 item typed continuous on the Ingest tab enters as ordered here", fixed = TRUE)
  expect_error(sieve_estimator_resolve(types, choice = "dwls"), "arg")
  # ML with FIML ("ml_fiml") is a choice, the second way forward the MLR
  # refusal names.
  expect_identical(unname(sieve_estimator_choices()), c("auto", "mlr", "ml_fiml", "ml", "wlsmv"))
})

# ---- 2. A private study of 68 items and the CFA tab's estimator control ------

test_that("a private study of 68 seven point items loaded from its data file resolves to ML with FIML, and the chip says so", {
  ethics <- siever_profile_file("ethics.csv")
  skip_if_not(file.exists(ethics), "the private study of 68 items is not available")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "ethics.csv", datapath = ethics))
    session$setInputs(load = 1L)
    expect_identical(nrow(state$meta), 68L)
    expect_identical(unique(state$meta$type), "continuous")
    expect_true(all(state$meta$n_levels == 7L))
    box <- df_html(output$estimator_box)
    expect_match(box, "ML with FIML", fixed = TRUE)
    expect_match(box, "For markedly non-normal items (strongly skewed or heavy-tailed), choose MLR with FIML", fixed = TRUE)
    expect_match(box, "Every included item is continuous (five or more scale points", fixed = TRUE)
    expect_match(box, "(n = 500)", fixed = TRUE)
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "ML with FIML")
    chip <- df_html(output$status_chip)
    expect_match(chip, "68 items loaded; types confirmed (ML with FIML)", fixed = TRUE)
    # Loaded from the data file alone the items have no construct (they are
    # Unassigned), which the count leaves out: Unassigned is not counted as
    # a construct.
    expect_match(df_html(output$confirm_button), "Confirmed: 68 continuous items, 0 constructs, 0 facets", fixed = TRUE)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "estimator = ML with FIML on a Pearson matrix", fixed = TRUE)
    expect_match(log, "Rationale:", fixed = TRUE)
  })
  # Every model specification of the study follows: ML with FIML, nothing ordered.
  spec <- sieve_cfa_spec(shiny::isolate(st$meta), reserved = names(shiny::isolate(st$raw)))
  expect_identical(spec$estimator, "ML")
  expect_identical(spec$missing, "fiml")
  expect_null(spec$ordered)
  expect_identical(sieve_lavaan_spec(shiny::isolate(st$meta))$estimator, "ML")
})

test_that("the CFA tab's Estimator control fits under the chosen estimator, keeps a fit per choice, and the model note states the choice with its reason", {
  st <- df_nine_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "auto", layout = "top_down")
    expect_identical(cfa_plan()$estimator, "ML")
    expect_identical(cfa_plan()$missing, "fiml")
    note <- df_html(output$model_note)
    expect_match(note, "estimated by ML with FIML", fixed = TRUE)
    expect_match(note, "Every included item is continuous", fixed = TRUE)
    session$setInputs(fit_go = 1L)
    auto <- cfa_run()
    expect_identical(auto$spec$estimator, "ML")
    expect_identical(auto$spec$missing, "fiml")
    expect_identical(auto$spec$estimator_choice, "auto")
    expect_identical(auto$status$code, "ok")
    # The default fit is lavaan's ML with FIML called directly. The study
    # declares age as an auxiliary variable, but every item is observed in
    # every row, so it adds no information and is left out: no warning, and
    # the nine items alone.
    expect_null(auto$spec$engine_args$aux)
    expect_length(auto$warnings, 0L)
    expect_setequal(lavaan::lavNames(auto$fit, "ov"), auto$spec$observed)
    direct_auto <- lavaan::cfa(auto$spec$model,
      data = shiny::isolate(state$items), estimator = "ML",
      std.lv = TRUE, missing = "ml"
    )
    expect_equal(lavaan::coef(direct_auto), lavaan::coef(auto$fit)[names(lavaan::coef(direct_auto))],
      tolerance = 1e-6
    )
    expect_identical(lavaan::lavInspect(auto$fit, "options")$estimator, "ML")
    # ML on the complete cases: another choice, its own fit.
    session$setInputs(estimator = "ml")
    expect_identical(cfa_plan()$estimator, "ML")
    expect_identical(demand$status()$state, "not_run")
    session$setInputs(fit_go = 2L)
    ml <- cfa_run()
    expect_identical(ml$spec$estimator, "ML")
    expect_identical(ml$spec$missing, "listwise")
    expect_identical(ml$spec$estimator_choice, "ml")
    expect_identical(lavaan::lavInspect(ml$fit, "options")$estimator, "ML")
    expect_match(df_html(output$model_note),
      "ML on the complete cases was chosen with the Estimator control",
      fixed = TRUE
    )
    # The same model fit directly: the estimates agree.
    direct <- lavaan::cfa(ml$spec$model,
      data = shiny::isolate(state$items), estimator = "ML",
      std.lv = TRUE, missing = "listwise"
    )
    expect_equal(lavaan::coef(direct), lavaan::coef(ml$fit), tolerance = 1e-8)
    # The recorded R says the estimator was chosen.
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "Estimator chosen with the Estimator control: ML on the complete cases", fixed = TRUE)
    expect_match(log, 'estimator = "ML"', fixed = TRUE)
    # WLSMV with every item as ordered: the note states the weight matrix.
    session$setInputs(estimator = "wlsmv")
    expect_identical(cfa_plan()$estimator, "WLSMV")
    note <- df_html(output$model_note)
    expect_match(note, "WLSMV was chosen with the Estimator control: every item is treated as ordered", fixed = TRUE)
    expect_match(note, "9 items typed continuous on the Ingest tab enter as ordered here", fixed = TRUE)
    expect_match(note, "The weight matrix of this fit holds", fixed = TRUE)
    expect_identical(demand$status()$state, "not_run")
    # Back to the default: the first fit is shown again without a refit.
    session$setInputs(estimator = "auto")
    expect_identical(demand$status()$state, "current")
    expect_identical(cfa_run()$spec$estimator, "ML")
    # A chosen ML with FIML is the same specification as the ML with FIML
    # the types imply; a chosen MLR is another.
    expect_identical(
      sieve_cfa_spec(shiny::isolate(state$meta), estimator = "ml_fiml")$key,
      sieve_cfa_spec(shiny::isolate(state$meta))$key
    )
    expect_false(identical(
      sieve_cfa_spec(shiny::isolate(state$meta), estimator = "mlr")$key,
      sieve_cfa_spec(shiny::isolate(state$meta))$key
    ))
  })
})

test_that("the WLSMV guard's remedy names the Estimator control, and the CFA tab shows it", {
  wide <- as.data.frame(matrix(rep_len(1:5, 40 * 303), 40, 303))
  names(wide) <- sprintf("item_%03d", seq_len(303))
  refused <- sieve_wlsmv_guard(names(wide), wide, estimator = "WLSMV")
  expect_match(refused,
    # (ML with FIML is a choice of the Estimator control and is what Follow
    # the item types gives continuous items.)
    "choose ML with FIML (or MLR with FIML for markedly non-normal items, or ML on the complete cases) in the Estimator control of the CFA tab's sidebar",
    fixed = TRUE
  )
  expect_match(refused, "treat the items as continuous and fit them by maximum likelihood", fixed = TRUE)
  # On the tab, with the limit lowered: the categorical choice is refused
  # with the control named, and the continuous choice is not refused.
  withr::local_options(sieve.wlsmv_limit = 1e4)
  st <- df_nine_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "wlsmv", layout = "top_down")
    expect_match(cfa_plan()$refused, "Estimator control of the CFA tab's sidebar", fixed = TRUE)
    expect_match(df_html(output$gate), "weight matrix of a WLSMV fit", fixed = TRUE)
    session$setInputs(estimator = "mlr")
    expect_null(cfa_plan()$refused)
  })
})

test_that("the Reliability tab's item treatment decides the coefficient without retyping the items", {
  expect_identical(
    sieve_reliability_treat(c("continuous", "ordered", "binary"), "auto"),
    c("continuous", "ordered", "binary")
  )
  expect_identical(
    sieve_reliability_treat(c("continuous", "ordered", "binary"), "continuous"),
    rep("continuous", 3L)
  )
  expect_identical(
    sieve_reliability_treat(c("continuous", "ordered", "binary"), "ordered"),
    c("ordered", "ordered", "binary")
  )
  expect_identical(unname(sieve_reliability_treatments()), c("auto", "continuous", "ordered"))
  st <- df_nine_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr", missing = "listwise",
      conf_level = 0.95, show_alpha = TRUE, B = 1000, treatment = "auto"
    )
    expect_identical(
      unname(vapply(estimable(), function(x) sieve_reliability_class(x$types), character(1))),
      rep("continuous", 3L)
    )
    expect_identical(rel_plan()$estimator, "MLR")
    session$setInputs(treatment = "ordered")
    expect_identical(
      unname(vapply(estimable(), function(x) sieve_reliability_class(x$types), character(1))),
      rep("categorical", 3L)
    )
    expect_identical(rel_plan()$estimator, "WLSMV")
    expect_identical(rel_choices()$treatment, "ordered")
    session$setInputs(treatment = "auto")
    expect_identical(rel_plan()$estimator, "MLR")
  })
})

# ---- 3. The question row ----------------------------------------------------

test_that("a question row under the header is read as the item wording, and the step 1 checkbox turns the reading off", {
  fx <- sieve_hs9_fixture()
  d <- fx$data[, c("id", fx$item_cols[1:4])]
  rows <- list(
    construct = c(
      t1_visual_perception = "Visual", t2_cubes = "Visual", t4_lozenges = "Visual",
      t6_paragraph_comprehension = "Textual"
    ),
    question = c(
      t1_visual_perception = "I can see the shapes", t2_cubes = "I can turn the cubes",
      t6_paragraph_comprehension = "I understand what I read"
    ),
    reverse = c(t2_cubes = "yes"),
    # A marked item whose values already correlate positively with its
    # construct is held rather than reversed a second time; the file says
    # it holds t2_cubes as administered, so it is reverse scored on loading.
    reverse_applied = c(t2_cubes = "no")
  )
  path <- df_described_csv(d, rows)
  expect_identical(sieve_description_words(), c("construct", "facet", "reverse", "reverse_applied", "question", "wording"))
  read <- sieve_read_data(path, "described.csv")
  expect_true(read$ok)
  expect_identical(
    read$description$wording,
    c("I can see the shapes", "I can turn the cubes", NA, "I understand what I read")
  )
  expect_identical(read$description$construct, c("Visual", "Visual", "Visual", "Textual"))
  expect_identical(nrow(read$value), nrow(d))
  expect_true(all(vapply(read$value[-1], is.numeric, logical(1))))
  # The recorded R takes the same rows off and gives the same data frame.
  env <- new.env()
  withr::with_dir(dirname(path), eval(parse(text = read$code), envir = env))
  expect_identical(env$d, read$value)
  expect_match(paste(read$code, collapse = " "), "question or wording", fixed = TRUE)
  # The row under its other name.
  alt <- df_described_csv(d, list(wording = rows$question), name = "wording.csv")
  expect_identical(sieve_read_data(alt, "wording.csv")$description$wording, read$description$wording)
  # Turned off: every row is data, so the columns are text and no item is proposed.
  off <- sieve_read_data(path, "described.csv", description = FALSE)
  expect_true(off$ok)
  expect_null(off$description)
  expect_identical(nrow(off$value), nrow(d) + 4L)
  expect_true(is.character(off$value$t2_cubes))
  expect_false(any(grepl("described <-", off$code, fixed = TRUE)))

  # On the tab: the wording is the Question, the Item Name stays the
  # variable, the chip says what was read; off, the file is refused as a
  # data file whose columns are text, and the chip says the rows were data.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "ready", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "described.csv", datapath = path))
    session$setInputs(load = 1L)
    m <- state$meta
    expect_identical(m$wording[m$item == "t1_visual_perception"], "I can see the shapes")
    expect_identical(m$label[m$item == "t1_visual_perception"], "t1_visual_perception")
    expect_identical(m$wording[m$item == "t4_lozenges"], "t4_lozenges")
    expect_true(m$reverse[m$item == "t2_cubes"])
    chip <- df_html(output$status_chip)
    expect_match(chip,
      "Constructs read from the file: 2 constructs, 0 facets; 1 item reverse scored as the reverse row says; the question row read for 3 items.",
      fixed = TRUE
    )
    expect_match(df_html(output$question_tbl), "I understand what I read", fixed = TRUE)
    session$setInputs(read_rows = FALSE)
    # With a study loaded, Load asks first; the replacement is then refused.
    session$setInputs(load = 2L)
    session$setInputs(load_replace = 1L)
    # Refused: nothing changed, the earlier study stands.
    expect_identical(state$meta$wording[state$meta$item == "t1_visual_perception"], "I can see the shapes")
  })
  # A file whose second row is data, loaded with the rows off, is read as data.
  plain <- df_path("plain.csv")
  utils::write.csv(d, plain, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = FALSE
    )
    session$setInputs(file = list(name = "plain.csv", datapath = plain))
    session$setInputs(load = 1L)
    expect_identical(nrow(state$items), nrow(d))
    expect_match(df_html(output$status_chip),
      "Rows under the header were read as data (the step 1 option to read description rows is off).",
      fixed = TRUE
    )
  })
  # The template and the pre-SIEVER step carry the questions.
  template <- sieve_specification_template(path)
  expect_identical(template$wording[template$variable == "t1_visual_perception"], "I can see the shapes")
  expect_identical(template$label[template$variable == "t1_visual_perception"], "")
  project <- df_path("described.siever.rds")
  invisible(utils::capture.output(study <- sieve_prepare_study(path, template, file = project)))
  expect_identical(study$meta$wording[study$meta$item == "t6_paragraph_comprehension"], "I understand what I read")
})

# ---- 4. The range warning and the study-wide declaration --------------------

test_that("an item whose observed range differs from the study's common range is named, and the bounds of every item can be declared at once", {
  withr::local_seed(8)
  d <- as.data.frame(lapply(1:8, function(j) sample(1:5, 120, TRUE)))
  names(d) <- sprintf("q%d", 1:8)
  d$q9 <- sample(1:7, 120, TRUE)
  d <- cbind(id = seq_len(120), d)
  # The check itself, on a ledger built from the data.
  m <- sieve_build_meta(d, names(d)[-1])
  check <- sieve_range_check(m)
  expect_identical(check$common, c(1, 5))
  expect_identical(check$n_common, 8L)
  expect_identical(check$n, 9L)
  expect_true(check$majority)
  expect_identical(check$differing$item, "q9")
  expect_identical(check$differing$min, 1)
  expect_identical(check$differing$max, 7)
  expect_match(sieve_range_text(check),
    "8 of the 9 included items span 1 to 5, but 1 has a different observed range: q9 (1 to 7)",
    fixed = TRUE
  )
  # No warning when every item shares the range, or when only one item has one.
  same <- sieve_build_meta(d[, 2:9], names(d)[2:9])
  expect_identical(nrow(sieve_range_check(same)$differing), 0L)
  expect_null(sieve_range_text(sieve_range_check(same)))
  expect_null(sieve_range_check(sieve_build_meta(d[, 2, drop = FALSE], "q1")))
  # An item with declared bounds is left alone.
  declared <- m
  declared$rev_lo[declared$item == "q9"] <- 1
  declared$rev_hi[declared$item == "q9"] <- 7
  declared$rev_bounds[declared$item == "q9"] <- "declared"
  expect_identical(nrow(sieve_range_check(declared)$differing), 0L)
  # No majority: the ranges are listed.
  half <- m
  half$max[1:3] <- 7
  half$max[8] <- 6 # 4 items per common range and one on a third range
  text <- sieve_range_text(sieve_range_check(half))
  expect_match(text, "no range shared by a majority", fixed = TRUE)
  expect_match(text, "1 to 7 for 4 items", fixed = TRUE)

  path <- df_path("ranges.csv")
  utils::write.csv(d, path, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "ranges.csv", datapath = path))
    session$setInputs(load = 1L)
    note <- df_html(output$bounds_note)
    expect_match(note, "Response bounds.", fixed = TRUE)
    expect_match(note, "q9 (1 to 7)", fixed = TRUE)
    # The declaration is the Response bounds row above the grid, or an item's
    # own cell; the note says where, with the study's common observed range.
    expect_match(note, "Declare the bounds of the scale under", fixed = TRUE)
    expect_match(note, "common observed range is 1 to 5", fixed = TRUE)
    # The warning repeats in the Response bounds cell and in the ledger's
    # Range column.
    grid <- paste(as.character(output$type_grid), collapse = "")
    expect_match(grid, "observed 1 to 7; the study's common range is 1 to 5 and no bounds are declared", fixed = TRUE)
    expect_identical(sum(gregexpr("common range is 1 to 5", grid, fixed = TRUE)[[1]] > 0), 1L)
    ledger <- paste(as.character(output$question_tbl), collapse = "")
    expect_match(ledger, "1 to 7 (observed); differs from the common range 1 to 5, no bounds declared", fixed = TRUE)
    expect_match(ledger, "1 to 5 (observed)", fixed = TRUE)
    # One item reverse scored within its observed range before the declaration.
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "Yes"))
    expect_identical(state$meta$rev_bounds[state$meta$item == "q2"], "observed")
    # The study-wide declaration: the Response bounds row, every included
    # item (the row's default scope).
    session$setInputs(bounds_lo = 1, bounds_hi = 5)
    session$setInputs(bounds_apply = 1L)
    m <- state$meta
    covered <- setdiff(m$item, c("q2", "q9"))
    expect_true(all(m$rev_bounds[m$item %in% covered] == "declared"))
    expect_true(all(m$rev_lo[m$item %in% covered] == 1))
    expect_true(all(m$rev_hi[m$item %in% covered] == 5))
    expect_identical(m$rev_bounds[m$item == "q2"], "observed")
    expect_true(is.na(m$rev_bounds[m$item == "q9"]))
    said <- df_html(output$grid_note)
    expect_match(said, "Response bounds 1 to 5 declared for 7 items", fixed = TRUE)
    expect_match(said, "q2 is reverse scored already", fixed = TRUE)
    expect_match(said, "q9 has a value outside 1 to 5", fixed = TRUE)
    # Nothing analytic changed: the confirmation state is as before, the
    # values are untouched, and the ledger write is recorded.
    expect_false(isTRUE(state$confirmed))
    expect_identical(as.numeric(state$items$q1), as.numeric(d$q1))
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("Declare the response bounds 1 to 5 for 7 items", log, fixed = TRUE)))
    expect_true(any(grepl('meta$rev_bounds[meta$item %in% declared_items] <- "declared"', log, fixed = TRUE)))
    # The warning says the one item not covered is on a scale of its own.
    expect_match(df_html(output$bounds_note),
      "1 is not covered because its observed range lies outside them: q9 (1 to 7)",
      fixed = TRUE
    )
    # A later reversal reflects within the declared bounds.
    session$setInputs(cell_edit = list(item = "q3", field = "reverse", value = "Yes"))
    m <- state$meta
    expect_identical(m$rev_bounds[m$item == "q3"], "declared")
    expect_identical(as.numeric(state$items$q3), 6 - as.numeric(d$q3))
    # The declaration reaches the specification export.
    spec <- sieve_spec_table(state$raw, state$items, state$meta, state$roles)
    expect_identical(spec$rev_bounds[spec$variable == "q1"], "declared")
    expect_identical(spec$rev_lo[spec$variable == "q1"], "1")
    expect_identical(spec$rev_hi[spec$variable == "q1"], "5")
    expect_identical(spec$rev_bounds[spec$variable == "q9"], "")
    # Bounds the wrong way round declare nothing.
    session$setInputs(bounds_lo = 5, bounds_hi = 1)
    session$setInputs(bounds_apply = 2L)
    expect_match(df_html(output$grid_note), "nothing was declared", fixed = TRUE)
  })
})

# ---- 5. The summary export ---------------------------------------------------

test_that("the summary export holds the polychoric matrix when it is the analysis basis, and nothing more with a Pearson basis", {
  skip_if_not_installed("psych")
  d <- df_ordered_frame()
  st <- sieve_new_state()
  st$dataset_name <- "three point items"
  st$raw <- d
  st$items <- d
  st$id_vars <- character(0)
  m <- sieve_build_meta(d, names(d), list(A = names(d)[1:3], B = names(d)[4:6]))
  expect_identical(unique(m$type), "ordered")
  st$meta <- m
  st$confirmed <- TRUE
  st$estimator <- "WLSMV (DWLS)"
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  shiny::isolate(sieve_new_dataset(st))
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(summary_means = TRUE, export_values = "raw")
    expect_identical(summary_file(), "siever_summary_processed_with_means_polychoric.csv")
    file <- output$dl_summary
    header <- readLines(file, n = 4L)
    expect_match(header[3],
      "# Correlations: the rows named polychoric.<item> hold the polychoric correlation matrix",
      fixed = TRUE
    )
    expect_match(header[3], "requested polychoric, used polychoric; complete cases, n = 240", fixed = TRUE)
    read <- utils::read.csv(file, comment.char = "#", row.names = 1, check.names = FALSE)
    expect_identical(rownames(read), c(names(d), "mean", paste0("polychoric.", names(d))))
    basis <- shiny::isolate(sieve_state_basis(state))
    expect_identical(basis$used, "polychoric")
    block <- as.matrix(read[paste0("polychoric.", names(d)), ])
    expect_equal(block, basis$R, tolerance = 1e-12, ignore_attr = TRUE)
    # Against psych directly.
    expect_equal(block, psych::polychoric(d, correct = 0.5, progress = FALSE)$rho,
      tolerance = 1e-8,
      ignore_attr = TRUE
    )
    expect_equal(as.matrix(read[names(d), ]), stats::cov(as.matrix(d)), tolerance = 1e-12, ignore_attr = TRUE)
    # Recorded, and the recorded R writes the same file from R_items.
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("covariance matrix and means and the polychoric correlation matrix, n = 240 complete cases",
      log,
      fixed = TRUE
    )))
    stamps <- which(startsWith(log, "# ["))
    at <- max(stamps[grepl("Export the summary", log[stamps], fixed = TRUE)])
    code <- log[(at + 1L):length(log)]
    env <- new.env()
    env$active_items <- d
    env$R_items <- basis$R
    dir <- withr::local_tempdir()
    withr::with_dir(dir, eval(parse(text = code[!startsWith(code, "#") & nzchar(code)]), envir = env))
    replayed <- utils::read.csv(file.path(dir, summary_file()),
      comment.char = "#", row.names = 1,
      check.names = FALSE
    )
    expect_identical(replayed, read)
    # Without the means the block still follows the matrix.
    session$setInputs(summary_means = FALSE)
    read2 <- utils::read.csv(output$dl_summary, comment.char = "#", row.names = 1, check.names = FALSE)
    expect_identical(rownames(read2), c(names(d), paste0("polychoric.", names(d))))
  })
  # The plain function: a basis that is not the pool's, or Pearson, adds nothing.
  s <- sieve_summary_table(d, m, means = FALSE, basis = list(ok = TRUE, used = "pearson", R = stats::cor(d)))
  expect_null(s$cor)
  s <- sieve_summary_table(d, m,
    means = FALSE,
    basis = list(ok = TRUE, used = "polychoric", R = stats::cor(d[, 1:3]), n = 240L)
  )
  expect_null(s$cor)
  # The nine tests: Pearson basis, no block, the header says so.
  st <- df_nine_state()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(summary_means = TRUE, export_values = "raw")
    expect_identical(summary_file(), "siever_summary_processed_with_means.csv")
    file <- output$dl_summary
    expect_match(readLines(file, n = 4L)[3], "the analysis basis is Pearson", fixed = TRUE)
    read <- utils::read.csv(file, comment.char = "#", row.names = 1, check.names = FALSE)
    expect_identical(rownames(read), c(sieve_hs9_fixture()$item_cols, "mean"))
  })
})

# ---- 6. Antecedents ----------------------------------------------------------

test_that("the role antecedent goes through the editor, the specification, the project file and the SEM predictor list, and is never a criterion", {
  expect_identical(sieve_role_levels(), c("item", "identifier", "group", "criterion", "antecedent", "auxiliary"))
  fx <- sieve_hs9_fixture()
  # Never proposed automatically.
  expect_false("antecedent" %in% sieve_propose_roles(fx$data)$role)
  st <- df_nine_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(roles_role = "antecedent", roles_names = "age_years, grade", roles_pick = character(0))
    session$setInputs(roles_apply = 1L)
    roles <- sieve_roles_table(state$raw, state$items, state$roles)
    expect_identical(roles$role[roles$variable %in% c("age_years", "grade")], rep("antecedent", 2L))
    expect_match(df_html(output$roles_editor_note), "Role antecedent set for 2 variables", fixed = TRUE)
    expect_match(df_html(output$roles_notice), "2 antecedents", fixed = TRUE)
    expect_match(df_html(output$status_chip), "2 antecedents", fixed = TRUE)
    expect_match(df_html(output$antecedent_note),
      "Antecedents (variables that exist before the target construct): grade, age_years",
      fixed = TRUE
    )
    expect_match(paste(as.character(output$roles_grid), collapse = ""), "Antecedent", fixed = TRUE)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "Set the role of age_years to antecedent (it was an auxiliary)", fixed = TRUE)
    expect_match(log, "# variable role: \"grade\" is antecedent", fixed = TRUE)
  })
  expect_identical(sieve_roles_summary(shiny::isolate(st$roles)), "9 items, 1 identifier, 2 groups, 2 antecedents")
  # The specification carries the role and reads it back.
  spec <- sieve_spec_table(
    shiny::isolate(st$raw), shiny::isolate(st$items), shiny::isolate(st$meta),
    shiny::isolate(st$roles)
  )
  expect_identical(spec$role[spec$variable == "age_years"], "antecedent")
  fresh <- df_nine_state()
  out <- sieve_apply_spec_table(fresh, spec, keying = "verify")
  expect_true(out$ok)
  expect_identical(sieve_roles_table(
    shiny::isolate(fresh$raw), shiny::isolate(fresh$items),
    shiny::isolate(fresh$roles)
  )$role[spec$variable == "grade"], "antecedent")
  # The project file keeps it.
  snapshot <- sieve_project_snapshot(st)
  expect_length(sieve_project_check(snapshot), 0L)
  path <- df_path("antecedents.siever.rds")
  sieve_project_write(snapshot, path)
  reopened <- sieve_new_state()
  sieve_project_restore(reopened, sieve_project_read(path), source = "antecedents.siever.rds")
  expect_identical(
    shiny::isolate(reopened$roles)$role[shiny::isolate(reopened$roles)$variable == "age_years"],
    "antecedent"
  )
  shiny::isolate(sieve_confirm_types(st))
  # The SEM tab offers it as a predictor (grade has two values: refused by
  # name); the fit holds the observed predictor, and agrees with lavaan.
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(layout = "left_right")
    expect_identical(ante_read()$offered, "age_years")
    expect_match(ante_read()$refused[["grade"]], "fewer than three distinct values", fixed = TRUE)
    session$setInputs(outcomes = "Speed", predictors = c("Visual", "antecedent:age_years"))
    expect_identical(sem_choices()$predictors, "Visual")
    expect_identical(sem_choices()$antecedents, "age_years")
    expect_match(sem_plan()$sentence, "6 items and 1 antecedent", fixed = TRUE)
    session$setInputs(fit_go = 1L)
    run <- path_run()
    expect_identical(run$status$code, "ok")
    expect_identical(attr(run$fit, "pred_map"), c(age_years = "age_years"))
    expect_match(run$spec$model, "Speed ~ Visual + age_years", fixed = TRUE)
    direct <- lavaan::sem(run$spec$model,
      data = cbind(shiny::isolate(state$items), age_years = shiny::isolate(state$raw$age_years)),
      estimator = "ML", std.lv = TRUE, missing = "ml"
    )
    expect_equal(lavaan::coef(direct), lavaan::coef(run$fit), tolerance = 1e-8)
    expect_identical(lavaan::lavInspect(run$fit, "options")$estimator, "ML")
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "Fit the structural model: Speed on Visual, age_years (antecedent)", fixed = TRUE)
    lines <- unlist(strsplit(log, "\n", fixed = TRUE))
    first <- which(lines == "sem_data <- items")[1L]
    last <- grep("^sem_fit <- lavaan::sem", lines)[1L] - 1L
    expect_true(is.finite(first) && is.finite(last) && first < last)
    preparation <- parse(text = lines[seq.int(first, last)])
    values <- c(10, 30, NA_real_, 20)
    for (input in list(values, as.character(values),
      factor(as.character(values), levels = c("30", "10", "20")))) {
      replay_input <- new.env(parent = baseenv())
      replay_input$d <- state$raw[seq_along(input), , drop = FALSE]
      replay_input$d$age_years <- input
      replay_input$items <- state$items[seq_along(input), , drop = FALSE]
      eval(preparation, replay_input)
      expect_identical(replay_input$sem_data$age_years, values)
    }
    expect_match(df_html(output$gate),
      "Not offered as a predictor: Antecedent 'grade' has fewer than three distinct values",
      fixed = TRUE
    )
    # The diagram draws the observed predictor.
    expect_true(sieve_diagram_drawable(diagram_base()))
  })
  # A mediation chain from an antecedent through a construct.
  m <- shiny::isolate(st$meta)
  plan <- sieve_structural_plan(m,
    outcomes = c("Textual", "Speed"), predictors = "Textual",
    antecedents = "age_years", reserved = names(fx$data), effects = TRUE
  )
  expect_length(plan$problems, 0L)
  expect_identical(unname(plan$eff_map["ind1"]), "age_years -> Textual -> Speed (indirect)")
  # Refusals: an antecedent that is an indicator, or an outcome.
  expect_match(
    sieve_structural_plan(m, "Speed", character(0),
      antecedents = fx$theory_map[["Speed"]][1L],
      reserved = names(fx$data)
    )$problems,
    "also an indicator in the measurement model",
    fixed = TRUE
  )
  expect_match(
    sieve_structural_plan(m, "age_years", "Visual",
      antecedents = "age_years",
      reserved = names(fx$data)
    )$problems,
    "cannot predict itself",
    fixed = TRUE
  )
  # The Validity tab does not offer an antecedent as a criterion.
  shiny::testServer(mod_validity_server, args = list(state = st), {
    expect_false("age_years" %in% aux_read()$offered)
    expect_false("age_years" %in% names(aux_read()$refused))
  })
  # The Screening summary lists it under its own heading.
  shiny::testServer(mod_screen_server, args = list(state = st), {
    expect_match(df_html(output$antecedent_note),
      "Antecedents (variables that exist before the target construct): grade, age_years",
      fixed = TRUE
    )
    expect_match(df_html(output$pool_note), "2 antecedents", fixed = TRUE)
  })
})

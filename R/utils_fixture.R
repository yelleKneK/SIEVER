#' Curated descriptions for the Holzinger and Swineford tests
#'
#' One source of truth for the Item Names used by both bundled examples, so
#' each test carries an informative description (the documented test names)
#' rather than a terse column stem. Names are the columns of DMAR's
#' `holzinger_swineford` data.
#'
#' @return A named character vector, column name -> description.
#' @author Ken Kelley
#' @noRd
sieve_hs_test_labels <- function() {
  c(
    t1_visual_perception = "Visual perception",
    t2_cubes = "Cubes (spatial visualization)",
    t3_paper_form_board = "Paper form board (spatial)",
    t4_lozenges = "Lozenges (spatial relations)",
    t5_general_information = "General information (verbal)",
    t6_paragraph_comprehension = "Paragraph comprehension",
    t7_sentence = "Sentence completion",
    t8_word_classification = "Word classification",
    t9_word_meaning = "Word meaning",
    t10_addition = "Speeded addition",
    t11_code = "Speeded coding (digit-symbol)",
    t12_counting_groups_of_dots = "Speeded counting of dots",
    t13_straight_and_curved_capitals = "Speeded discrimination of straight and curved capitals",
    t14_word_recognition = "Word recognition (memory)",
    t15_number_recognition = "Number recognition (memory)",
    t16_figure_recognition = "Figure recognition (memory)",
    t17_object_number = "Object-number memory",
    t18_number_figure = "Number-figure memory",
    t19_figure_word = "Figure-word memory",
    t20_deduction = "Deduction (reasoning)",
    t21_numerical_puzzles = "Numerical puzzles",
    t22_problem_reasoning = "Problem reasoning",
    t23_series_completion = "Series completion",
    t24_woody_mccall = "Woody-McCall mixed fundamentals (arithmetic)",
    t25_paper_form_board_r = "Paper form board (retest)",
    t26_flags = "Flags (spatial)"
  )
}

#' Label item columns from the curated map, falling back to a cleaned stem
#'
#' @param item_cols Character vector of column names.
#' @return A named character vector of labels.
#' @author Ken Kelley
#' @noRd
sieve_hs_labels_for <- function(item_cols) {
  map <- sieve_hs_test_labels()
  out <- unname(map[item_cols])
  miss <- is.na(out)
  if (any(miss)) {
    stem <- sub("^t[0-9]+_", "", item_cols[miss])
    out[miss] <- sub("^(.)", "\\U\\1", gsub("_", " ", stem), perl = TRUE)
  }
  stats::setNames(out, item_cols)
}

#' The supported Holzinger and Swineford data, validated
#'
#' The single point at which SIEVER reads the Holzinger and Swineford (1939)
#' data from DMAR. DMAR ships the data under one documented name,
#' `holzinger_swineford`.
#' This adapter reads the supported object and then checks the properties
#' the bundled examples, the vignettes, and the tests rely on, so that a
#' change in the upstream data is reported as such rather than surfacing as
#' an unrelated failure downstream:
#'
#' * 301 pupils, one row each;
#' * the identifying columns `id`, `sex`, `grade`, `age_years`, `school`;
#' * the 24 complete tests `t1_...` through `t24_...` under the names in
#'   `sieve_hs_test_labels()`, numeric, with no missing values;
#' * `school` naming exactly the Pasteur and Grant-White schools.
#'
#' The tests are raw scores. They are **not** the rescaled `x1` to `x9` of
#' lavaan's `HolzingerSwineford1939`, which divides each of the nine tests
#' by a constant (6, 4, 8, 3, 4, 7, 23, 20, 36 respectively); correlations
#' and standardized solutions agree, covariances and unstandardized
#' loadings do not.
#'
#' @return A data frame, or `NULL` when DMAR is not installed.
#' @author Ken Kelley
#' @noRd
sieve_hs_data <- function() {

  d <- tryCatch(
    getExportedValue("DMAR", "holzinger_swineford"),
    error = function(e) NULL
  )
  sieve_hs_validate(d, version = as.character(utils::packageVersion("DMAR")))
}

#' Check that a data frame is the Holzinger and Swineford data SIEVER expects
#'
#' The checks behind `sieve_hs_data()`, kept separate from the read so they
#' can be exercised on deliberately wrong input.
#'
#' @param d The object read from DMAR (a data frame when all is well).
#' @param version Character, the DMAR version, named in the error message.
#' @return `d`, unchanged, when every check passes; otherwise an error that
#'   names the property that failed.
#' @author Ken Kelley
#' @noRd
sieve_hs_validate <- function(d, version = "unknown") {
  bad <- function(why) {
    stop(sprintf(
      paste0(
        "The Holzinger and Swineford data in the installed DMAR (version %s) ",
        "do not have the form SIEVER's bundled examples require: %s. ",
        "SIEVER needs DMAR 1.1.0 or later."
      ),
      version, why
    ), call. = FALSE)
  }
  if (!is.data.frame(d)) {
    bad("the data object `holzinger_swineford` was not found")
  }
  tests24 <- names(sieve_hs_test_labels())[1:24]
  need <- c("id", "sex", "grade", "age_years", "school", tests24)
  miss <- setdiff(need, names(d))
  if (length(miss) > 0L) {
    bad(sprintf("missing column(s) %s", paste(miss, collapse = ", ")))
  }
  if (nrow(d) != 301L) {
    bad(sprintf("%d rows were found where 301 pupils are expected", nrow(d)))
  }
  numeric_ok <- vapply(d[tests24], is.numeric, logical(1))
  if (!all(numeric_ok)) {
    bad(sprintf(
      "non-numeric test column(s) %s",
      paste(tests24[!numeric_ok], collapse = ", ")
    ))
  }
  if (anyNA(d[tests24])) {
    bad("missing values in the 24 complete tests")
  }
  if (!setequal(unique(as.character(d$school)), c("Pasteur", "Grant-White"))) {
    bad("`school` does not name exactly the Pasteur and Grant-White schools")
  }
  d
}

#' The full Holzinger and Swineford test fixture (24 tests, both schools)
#'
#' Assembles a bundled starter example from DMAR's `holzinger_swineford`
#' data (through `sieve_hs_data()`): all 301 children
#' (both the Pasteur and Grant-White schools) on the 24 clean tests
#' (t1 to t24), with five a priori ability domains seeded as the theory and
#' curated Item Names. Because both schools are present, the `school`
#' variable supports a measurement-invariance demonstration. The famous
#' three factor structure is a target the winnowing can arrive at, not the
#' starting point.
#'
#' Requires DMAR (for the data); returns `NULL` if it is not installed.
#'
#' @return A fixture list (data, item columns, auxiliary variables, seeded
#'   theory map, theorized number of factors, label, and `labels`), or
#'   `NULL` when DMAR is unavailable.
#' @author Ken Kelley
#' @noRd
sieve_hs_fixture <- function() {
  d <- sieve_hs_data()
  if (is.null(d)) {
    return(NULL)
  }
  items24 <- grep("^t([1-9]|1[0-9]|2[0-4])_", names(d), value = TRUE)
  keep_id <- intersect(c("id", "sex", "grade", "age_years", "school"), names(d))
  df <- d[, c(keep_id, items24)]
  # Curated, documented Item Names (shared with the primary-9 example).
  labels <- sieve_hs_labels_for(items24)
  theory_map <- list(
    Spatial = grep("^t[1-4]_", items24, value = TRUE),
    Verbal = grep("^t[5-9]_", items24, value = TRUE),
    Speed = grep("^t1[0-3]_", items24, value = TRUE),
    Memory = grep("^t1[4-9]_", items24, value = TRUE),
    Reasoning = grep("^t2[0-4]_", items24, value = TRUE)
  )
  list(
    name = "Holzinger and Swineford, full (n = 301, 24 tests, both schools)",
    data = df,
    item_cols = items24,
    load_code = c("library(DMAR)", "d <- DMAR::holzinger_swineford  # full, both schools",
      'items <- grep("^t([1-9]|1[0-9]|2[0-4])_", names(d), value = TRUE)',
      'd <- d[, c("id", "sex", "grade", "age_years", "school", items)]'),
    id_vars = keep_id,
    theory_map = theory_map,
    theory_k = 5L,
    theory_lbl = "5 a priori ability domains (target solution: 3 factors)",
    labels = labels
  )
}

#' Load a Declared Example Through the Study Preparation Services
#' @param fixture A declared data fixture with item columns and a theory map.
#' @param state Existing state, or a new state by default.
#' @param confirmed Whether to confirm the declared item types.
#' @param labels Whether to apply the fixture's display labels.
#' @param theory Whether to retain its theoretical structure.
#' @param name Dataset display name.
#' @return Shared study state.
#' @author Ken Kelley
#' @noRd
sieve_study_state <- function(fixture = sieve_hs9_fixture(), state = sieve_new_state(),
                              confirmed = TRUE, labels = TRUE, theory = TRUE,
                              name = fixture$name) {
  shiny::isolate({
    loaded <- sieve_load_frame(state, fixture$data, name, record = FALSE)
    if (!isTRUE(loaded$ok)) stop(loaded$problem, call. = FALSE)
    state$roles <- sieve_declared_roles(fixture$data, fixture$item_cols)
    state$held_rows <- list()
    state$items <- fixture$data[, fixture$item_cols, drop = FALSE]
    sieve_write_ledger(state, sieve_build_meta(fixture$data, fixture$item_cols,
      fixture$theory_map, labels = if (labels) fixture$labels else NULL))
    state$theory <- if (theory) list(k = fixture$theory_k,
      label = fixture$theory_lbl, map = fixture$theory_map) else NULL
    sieve_log(state, sprintf("Load bundled example: %s", name), fixture$load_code, kind = "import")
    if (confirmed) sieve_confirm_types(state, origin = "auto")
    state
  })
}

#' The primary nine Holzinger and Swineford tests (3 factors), from DMAR
#'
#' The widely used nine variable subset (the textbook CFA example, the same
#' nine that lavaan ships as `HolzingerSwineford1939`'s x1 to x9), but the
#' item columns are pulled from the full DMAR `holzinger_swineford` data
#' so both bundled examples share one data source. The nine map onto the lavaan variables
#' as confirmed by exact proportionality (each lavaan variable is the DMAR
#' raw score divided by a constant; see `sieve_hs_data()`): x1->t1, x2->t2,
#' x3->t4, x4->t6, x5->t7, x6->t9, x7->t10, x8->t12, x9->t13. Item Names carry the
#' descriptions from the lavaan help file. The a priori structure is the
#' three factors Visual, Textual, Speed.
#'
#' Requires DMAR; returns `NULL` if it is not installed.
#'
#' @return A fixture list (see `sieve_hs_fixture()`) with a `labels`
#'   element, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_hs9_fixture <- function() {
  d <- sieve_hs_data()
  if (is.null(d)) {
    return(NULL)
  }
  items <- c(
    "t1_visual_perception", "t2_cubes", "t4_lozenges",
    "t6_paragraph_comprehension", "t7_sentence", "t9_word_meaning",
    "t10_addition", "t12_counting_groups_of_dots",
    "t13_straight_and_curved_capitals"
  )
  labels <- sieve_hs_labels_for(items) # curated, shared with the full example
  keep_id <- intersect(c("id", "sex", "grade", "age_years", "school"), names(d))
  df <- d[, c(keep_id, items)]
  theory_map <- list(
    Visual = items[1:3],
    Textual = items[4:6],
    Speed = items[7:9]
  )
  list(
    name = "Holzinger and Swineford, primary 9 tests (n = 301, from DMAR)",
    data = df,
    item_cols = items,
    load_code = c("library(DMAR)", "d <- DMAR::holzinger_swineford  # primary 9 tests (lavaan x1-x9, raw scores)",
      paste0("items <- ", sieve_r_literal(items)),
      'd <- d[, c("id", "sex", "grade", "age_years", "school", items)]'),
    id_vars = keep_id,
    theory_map = theory_map,
    theory_k = 3L,
    theory_lbl = "3 a priori factors: Visual, Textual, Speed",
    labels = labels
  )
}

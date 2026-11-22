# Export and import, the project snapshot, and variable roles.
#
# The expected values are written out by hand, follow from the algebra of a
# reflection, or come from an engine that is not the function under test
# (DMAR's invariance ladder on the bundled data, for the numeric grouping
# variable). The module tests drive the real observers with
# shiny::testServer: files are uploaded through the load observer, exported
# through the download handlers and read back through the import observer.
# A refused import or project is checked by comparing everything in the
# shared state, and the revision, before and after.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

ei_quiet <- function(expr) invisible(utils::capture.output(expr))

# A survey file: a numeric identifier, a numeric group, an outcome with a
# telling name (gpa), a criterion without one (perf), a text column, six
# five point items and one item with non-integer responses. Every value is
# given by a fixed rule, with no random numbers.
ei_survey <- function() {
  k <- seq_len(60L)
  likert <- function(shift, by) as.integer(((k * by + shift) %% 5L) + 1L)
  d <- data.frame(
    id = 1000L + k,
    sex = as.integer(k %% 2L) + 1L,
    gpa = 2 + ((k * 7L) %% 20L) / 10,
    perf = 40 + ((k * 13L) %% 37L) + (k %% 3L) / 4,
    site = c("north", "south", "east")[(k %% 3L) + 1L],
    q1 = likert(0L, 1L), q2 = likert(1L, 2L), q3 = likert(2L, 3L),
    q4 = likert(3L, 4L), q5 = likert(4L, 6L), q6 = likert(0L, 7L),
    q7 = (k %% 9L) / 7 + 0.1,
    stringsAsFactors = FALSE
  )
  d$q2[c(5L, 17L)] <- NA
  d
}

ei_csv <- function(d, name = "survey.csv") {
  path <- file.path(tempfile("ei_"), name)
  dir.create(dirname(path))
  utils::write.csv(d, path, row.names = FALSE)
  path
}

# Everything in the shared state a refused action has to leave as it was.
ei_whole <- function(state) {
  shiny::isolate(list(
    raw = state$raw, items = state$items, meta = state$meta, roles = state$roles,
    id_vars = sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), theory = state$theory, confirmed = state$confirmed,
    estimator = state$estimator, transforms = state$transforms, analyses = state$analyses,
    codelog = state$codelog, dataset_id = state$dataset_id, dataset_name = state$dataset_name,
    thresholds = state$thresholds, project = state$project
  ))
}

# A saved record with its dataset identity rewritten, as reopening a project
# rewrites the records of the saved dataset (written here by hand, apart
# from the package's own helper).
ei_relabel <- function(records, from, to) {
  lapply(records, function(r) {
    if (identical(r$dataset_id, from)) {
      r$dataset_id <- to
    }
    for (part in c("revision", "revision_before")) {
      if (!is.null(r[[part]]) && identical(r[[part]]$dataset_id, from)) {
        r[[part]]$dataset_id <- to
      }
    }
    r
  })
}

# Upload a file through the real load observer of an Ingest session.
ei_load <- function(session, path, name = basename(path), press = 1L) {
  session$setInputs(source = "file", basis = "theoretical", palette = "nd")
  session$setInputs(file = list(name = name, datapath = path))
  # With a study in the session a data file replaces it, and Load asks
  # first; the helper answers with the dialog's Load and replace.
  replacing <- identical(shiny::isolate(session$env$load_effect()), "replace")
  session$setInputs(load = press)
  if (replacing) session$setInputs(load_replace = press)
}

# The exports of a state through the Reproducible Code tab's Export panel,
# which holds the data, specification, summary and project downloads.
# Returns copies of the files,
# named data, spec, summary and project, for the kind of values asked for.
ei_export <- function(st, values = "raw", means = TRUE) {
  out <- NULL
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(export_values = values, summary_means = means)
    keep <- function(from, ext) {
      to <- tempfile(fileext = ext)
      file.copy(from, to)
      to
    }
    out <<- list(
      data = keep(output$dl_data, ".sav"),
      spec = keep(output$dl_spec, ".csv"),
      summary = keep(output$dl_summary, ".csv"),
      project = keep(output$save_project, ".rds")
    )
  })
  out
}

# A loaded state on the survey file, with its roles as proposed.
ei_state <- function(d = ei_survey()) {
  st <- sieve_new_state()
  path <- ei_csv(d)
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, path)
  })
  st
}

# The ledger columns an export and import round trip has to give back.
ei_ledger_cols <- c(
  "item", "label", "wording", "type", "construct", "facet", "status",
  "reverse", "rev_lo", "rev_hi", "rev_bounds",
  "mean", "sd", "min", "max", "n_missing", "n_levels"
)

# ---- Variable roles: the proposal -------------------------------------------

test_that("roles are proposed from the name and the values, and a column that is not a number is never an item", {
  k <- 1:24
  d <- data.frame(
    id = 500L + k, # named, every value different
    v7 = rev(k), # consecutive whole numbers, no telling name
    sex = rep(1:2, 12), # two values, named
    Gender = rep(c("m", "f", "x"), 8), # text, three values
    school_id = rep(1:4, 6), # grouping name plus a neutral word
    school_belonging_3 = rep(1:5, length.out = 24), # an item about school
    grade = rep(1:12, 2), # grouping name, too many values
    age_years = 20 + (k %% 11), # background name
    gpa = 2 + (k %% 13) / 10, # outcome name
    q1 = rep(c(1, 2, 3, 4, 5, 3), 4), # an item
    score = k * 1.5 + 0.25, # every value different, not whole: an item
    name = paste0("person", k), # text, every value different
    notes = rep(paste0("n", 1:8), 3), # text, eight values
    flag = rep(c(TRUE, FALSE), 12), # logical
    group = 3, # grouping name, one value
    stringsAsFactors = FALSE
  )
  roles <- SIEVER:::sieve_propose_roles(d)
  # Written out by hand from the rules in the documentation of the function.
  expected <- c(
    id = "identifier", v7 = "identifier", sex = "group", Gender = "group",
    school_id = "group", school_belonging_3 = "item", grade = "auxiliary",
    age_years = "auxiliary", gpa = "criterion", q1 = "item", score = "item",
    name = "identifier", notes = "auxiliary", flag = "group", group = "auxiliary"
  )
  expect_identical(stats::setNames(roles$role, roles$variable), expected)
  expect_identical(roles$variable, names(d))
  expect_identical(roles$proposed, roles$role)
  expect_true(all(nzchar(roles$reason)))
  expect_true(all(roles$role %in% SIEVER:::sieve_role_levels()))
  expect_identical(roles$numeric, unname(vapply(d, is.numeric, logical(1))))
  expect_false(any(roles$role[!roles$numeric] == "item"))
  expect_identical(roles$n_distinct[roles$variable == "notes"], 8L)

  # The words of a name are read whatever its case and separators.
  expect_identical(SIEVER:::sieve_name_words("Subject_ID2"), c("subject", "id"))
  expect_identical(SIEVER:::sieve_name_words("subjectId"), c("subject", "id"))
  expect_identical(SIEVER:::sieve_name_words("x.12"), "x")

  # The survey file used below: the outcome named gpa is proposed as a
  # criterion, the one named perf as an item, and the numeric identifier
  # and group are kept out of the item pool.
  s <- SIEVER:::sieve_propose_roles(ei_survey())
  expect_identical(
    stats::setNames(s$role, s$variable),
    c(
      id = "identifier", sex = "group", gpa = "criterion", perf = "item",
      site = "group", q1 = "item", q2 = "item", q3 = "item", q4 = "item",
      q5 = "item", q6 = "item", q7 = "item"
    )
  )
})

test_that("a role change moves a column in and out of the item pool and back to exactly where it was", {
  d <- ei_survey()
  roles <- SIEVER:::sieve_propose_roles(d)
  pool <- roles$variable[roles$role == "item"]
  items <- d[, pool, drop = FALSE]
  # The ledger as the load builds it for a file without variable labels.
  meta <- sieve_build_meta(d, pool, labels = stats::setNames(pool, pool))
  keys <- c(SIEVER:::sieve_data_key(items), SIEVER:::sieve_meta_key(meta))

  out <- SIEVER:::sieve_set_roles(d, items, meta, roles, c(perf = "criterion", sex = "auxiliary"))
  expect_identical(names(out$items), setdiff(pool, "perf"))
  expect_identical(out$meta$item, setdiff(pool, "perf"))
  expect_identical(out$id_vars, c("id", "sex", "gpa", "perf", "site"))
  expect_identical(out$roles$role[match(c("perf", "sex"), out$roles$variable)], c("criterion", "auxiliary"))
  expect_identical(out$changed, data.frame(
    variable = c("perf", "sex"), from = c("item", "group"),
    to = c("criterion", "auxiliary"), stringsAsFactors = FALSE
  ))
  expect_true(out$pool_changed)
  # The other items and their ledger rows are untouched.
  expect_identical(out$items, items[, setdiff(pool, "perf"), drop = FALSE])
  expect_identical(out$meta, SIEVER:::sieve_meta_keying(meta)[meta$item != "perf", ])
  # A change among the roles that are not item leaves the pool alone.
  only <- SIEVER:::sieve_set_roles(d, items, meta, roles, c(sex = "criterion"))
  expect_false(only$pool_changed)
  expect_identical(only$items, items)
  # The inputs are not modified.
  expect_identical(names(items), pool)

  # Back to an item: the column returns to its place with its values as
  # loaded, and its ledger row is the row a ledger built from the data holds.
  back <- SIEVER:::sieve_set_roles(d, out$items, out$meta, out$roles, c(perf = "item"))
  expect_identical(back$items, items)
  expect_identical(back$meta, SIEVER:::sieve_meta_keying(meta))
  expect_identical(c(SIEVER:::sieve_data_key(back$items), SIEVER:::sieve_meta_key(back$meta)), keys)
  # A numeric group can become an item, and takes the place its column has.
  as_item <- SIEVER:::sieve_set_roles(d, items, meta, roles, c(sex = "item"))
  expect_identical(names(as_item$items), c("sex", pool))
  expect_identical(as_item$items$sex, d$sex)
  expect_identical(as_item$meta$type[1], "binary")
})

test_that("a role change that cannot be made is refused with its reason and changes nothing", {
  d <- ei_survey()
  roles <- SIEVER:::sieve_propose_roles(d)
  pool <- roles$variable[roles$role == "item"]
  items <- d[, pool, drop = FALSE]
  meta <- sieve_build_meta(d, pool)
  set <- function(changes, items. = items, meta. = meta) {
    SIEVER:::sieve_set_roles(d, items., meta., roles, changes)
  }
  expect_error(set(c(site = "item")), "not stored as a number", class = "sieve_role_error")
  expect_error(set(c(nobody = "group")), "not in the data", class = "sieve_role_error")
  expect_error(set(c(sex = "covariate")), "not a variable role", class = "sieve_role_error")
  expect_error(set(stats::setNames(rep("group", length(pool)), pool)),
    "At least one variable has to be an item",
    class = "sieve_role_error"
  )
  # An item that was reverse scored here keeps its role until it is returned.
  r <- SIEVER:::sieve_reverse_item(items, meta, "q3", lo = 1, hi = 5, loaded = d$q3)
  expect_error(set(c(q3 = "auxiliary"), r$data, r$meta), "reverse scored here",
    class = "sieve_role_error"
  )
  # So does one recorded as already reverse scored in the data as loaded.
  m <- SIEVER:::sieve_mark_reversed(items, meta, "q3", 1, 5)
  expect_error(set(c(q3 = "auxiliary"), items, m$meta), "already reverse scored",
    class = "sieve_role_error"
  )
  # One refused change refuses the whole request: nothing was applied.
  expect_error(set(c(perf = "criterion", site = "item")), class = "sieve_role_error")
})

# ---- Variable roles: the upload, the grid, and the tabs that read them ------

test_that("an uploaded numeric identifier, group and criterion stay out of the item pool, and the proposal waits for review", {
  d <- ei_survey()
  path <- ei_csv(d)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, path)
    expect_identical(names(state$items), c("perf", paste0("q", 1:7)))
    expect_identical(state$meta$item, names(state$items))
    expect_identical(sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), c("id", "sex", "gpa", "site"))
    expect_identical(state$roles$variable, names(d))
    expect_identical(
      state$roles$role[match(c("id", "sex", "gpa", "perf"), state$roles$variable)],
      c("identifier", "group", "criterion", "item")
    )
    # The numeric columns keep their numbers: nothing was recoded as text.
    expect_identical(state$raw$sex, d$sex)
    expect_true(is.numeric(state$raw$gpa))
    # An uploaded file is not confirmed without a review of its roles ...
    expect_false(isTRUE(state$confirmed))
    expect_match(output$roles_notice$html, "Review the variable roles before you confirm")
    expect_match(output$roles_notice$html, "id \\(identifier\\), sex \\(group\\), gpa \\(criterion\\)")
    expect_match(paste(state$codelog, collapse = "\n"), "Variable roles proposed on reading, for review")
    # ... and the grid shows every column with its proposed role and the reason.
    grid <- output$roles_grid
    expect_true(all(vapply(names(d), function(v) grepl(sprintf(">%s<|\"%s\"", v, v), grid), logical(1))))

    # The researcher changes the role of the criterion that has no telling name.
    before <- SIEVER:::sieve_revision(state)
    ledger_before <- state$meta
    items_before <- state$items
    session$setInputs(cell_edit = list(item = "perf", field = "role", value = "Criterion"))
    expect_identical(names(state$items), paste0("q", 1:7))
    expect_identical(state$meta$item, paste0("q", 1:7))
    expect_identical(sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), c("id", "sex", "gpa", "perf", "site"))
    expect_identical(state$roles$role[state$roles$variable == "perf"], "criterion")
    expect_identical(state$roles$proposed[state$roles$variable == "perf"], "item")
    after <- SIEVER:::sieve_revision(state)
    # Dependent results are out of date by the revision: the data and the ledger both changed.
    expect_false(SIEVER:::sieve_same_revision(before, after, what = "data"))
    expect_false(SIEVER:::sieve_same_revision(before, after, what = "meta"))
    expect_match(SIEVER:::sieve_stale_reason(before, after), "item data have changed")
    expect_match(
      paste(state$codelog, collapse = "\n"),
      "Set the role of perf to criterion \\(it was an item\\)"
    )
    expect_identical(state$raw$perf, d$perf) # the data as loaded keep it
    expect_identical(SIEVER:::sieve_role_vars(state, "criterion"), c("gpa", "perf"))
    expect_identical(SIEVER:::sieve_role_vars(state, c("group", "identifier")), c("id", "sex", "site"))

    # Confirm, then change the pool: the confirmation is withdrawn.
    session$setInputs(confirm = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_match(
      paste(state$codelog, collapse = "\n"),
      "# Not items: id \\(identifier\\), sex \\(group\\), gpa \\(criterion\\), perf \\(criterion\\), site \\(group\\)\\."
    )
    session$setInputs(cell_edit = list(item = "sex", field = "role", value = "Criterion", n = 1L))
    expect_true(isTRUE(state$confirmed)) # the pool did not change
    session$setInputs(cell_edit = list(item = "perf", field = "role", value = "Item", n = 2L))
    expect_false(isTRUE(state$confirmed))
    # Changed and changed back: the same inputs as at the start, by content,
    # and the same item data and ledger.
    expect_true(SIEVER:::sieve_same_revision(before, SIEVER:::sieve_revision(state)))
    expect_identical(state$items, items_before)
    expect_identical(state$meta, ledger_before)

    # A change that cannot be made changes nothing at all.
    whole <- ei_whole(state)
    session$setInputs(cell_edit = list(item = "site", field = "role", value = "Item", n = 3L))
    expect_identical(ei_whole(state), whole)
    session$setInputs(cell_edit = list(item = "q3", field = "reverse", value = "Yes", n = 4L))
    whole <- ei_whole(state)
    session$setInputs(cell_edit = list(item = "q3", field = "role", value = "Group", n = 5L))
    expect_identical(ei_whole(state), whole)
  })
})

test_that("a bundled example declares its roles, keeps its non item columns, and still confirms on load", {
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    ei_quiet(session$setInputs(load = 1L))
    expect_identical(sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), fx$id_vars)
    expect_identical(state$roles$variable, names(fx$data))
    expect_identical(
      state$roles$role[match(fx$id_vars, state$roles$variable)],
      c("identifier", "group", "group", "auxiliary", "group")
    )
    expect_identical(state$roles$variable[state$roles$role == "item"], fx$item_cols)
    expect_true(isTRUE(state$confirmed))
    expect_match(output$roles_notice$html, "Variable roles:")
    expect_false(grepl("Review the variable roles", output$roles_notice$html, fixed = TRUE))
  })
})

test_that("a file that cannot be used leaves the loaded project as it is", {
  st <- ei_state()
  before <- ei_whole(st)
  only_text <- ei_csv(data.frame(name = c("a", "b", "c"), city = c("x", "y", "x")), "text_only.csv")
  not_data <- file.path(dirname(only_text), "notes.xyz")
  writeLines("not a data file", not_data)
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, only_text, press = 1L)
    expect_identical(ei_whole(state), before)
    ei_load(session, not_data, press = 2L)
    expect_identical(ei_whole(state), before)
  })
})

test_that("a numeric group and a numeric criterion are offered where they are used, and the numeric group gives the analysis of the labeled one", {
  # The bundled data with every non item column stored as a number, as a
  # survey export would have them: school and sex as codes.
  fx <- sieve_hs9_fixture()
  d <- fx$data
  d$sex <- as.integer(d$sex)
  d$school <- as.integer(d$school)
  path <- ei_csv(d, "hs_numeric.csv")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, path)
    expect_identical(names(state$items), fx$item_cols)
    expect_identical(
      state$roles$role[match(fx$id_vars, state$roles$variable)],
      c("identifier", "group", "group", "auxiliary", "group")
    )
    expect_true(is.numeric(state$raw$school))
    # The constructs of the example, so the confirmatory model is the same.
    for (cn in names(fx$theory_map)) {
      for (it in fx$theory_map[[cn]]) {
        session$setInputs(cell_edit = list(item = it, field = "construct", value = cn))
      }
    }
    session$setInputs(confirm = 1L)
    expect_true(isTRUE(state$confirmed))
  })
  shiny::testServer(mod_validity_server, args = list(state = st), {
    expect_true("age_years" %in% aux_numeric())
    expect_false(any(fx$item_cols %in% aux_numeric()))
  })
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    expect_identical(eligible_groups(), c("sex", "grade", "school"))
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    # The reference: DMAR's ladder on the bundled data with school as the
    # labeled factor it is shipped as. Recoding the labels as numbers does
    # not change a multiple group likelihood.
    cols <- fx$item_cols
    ref_data <- fx$data[, c(cols, "school")]
    ref_data <- ref_data[stats::complete.cases(ref_data), ]
    meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    # The study's default for continuous items is DMAR's default, ML with
    # FIML.
    ref <- DMAR::measurement_invariance(ref_data,
      model = sieve_cfa_plan(meta)$model, group = "school",
      estimator = "ML", missing = "fiml", std.lv = TRUE
    )
    expect_equal(r$table$chi_square, ref$chi_square, tolerance = 1e-8)
    expect_equal(r$table$df, ref$df)
    expect_equal(r$table$cfi, ref$cfi, tolerance = 1e-8)
  })
})

# ---- The export contract ----------------------------------------------------

test_that("a content key depends on the values and not on which missing value marker a number carries", {
  # A missing value that arithmetic has passed through differs from a
  # missing value as read in bits that identical() ignores by default.
  x <- c(1, NA, 3)
  y <- 6 - (6 - x)
  expect_identical(y, x)
  expect_false(identical(y, x, single.NA = FALSE))
  expect_identical(SIEVER:::sieve_values_key(y), SIEVER:::sieve_values_key(x))
  expect_identical(SIEVER:::sieve_data_key(data.frame(a = y)), SIEVER:::sieve_data_key(data.frame(a = x)))
  # Integer against double storage does not change it either; a value does.
  expect_identical(SIEVER:::sieve_values_key(c(1L, NA, 3L)), SIEVER:::sieve_values_key(x))
  expect_false(identical(SIEVER:::sieve_values_key(c(1, NA, 3 + 1e-12)), SIEVER:::sieve_values_key(x)))
})

test_that("the specification records status, type, construct, facet, keying, exact bounds, and what the data file holds", {
  d <- data.frame(
    id = 11:16, a = c(1L, 2L, 7L, NA, 4L, 6L), b = c(0.5, 1.5, 3.75, 2, NA, 0.75),
    c = c(2, 2, 3, 1, 1, 3)
  )
  items <- d[, c("a", "b", "c")]
  meta <- sieve_build_meta(d, c("a", "b", "c"))
  meta$construct <- c("Alpha", "Alpha", "Beta")
  meta$facet <- c("one", NA, NA)
  meta$status[3] <- "parked"
  # a: declared bounds 1 to 7. b: bounds that no short decimal identifies.
  lo_b <- 0.1 + 0.2
  hi_b <- 4 + 1 / 3
  r <- SIEVER:::sieve_reverse_item(items, meta, "a", lo = 1, hi = 7, loaded = d$a)
  r <- SIEVER:::sieve_reverse_item(r$data, r$meta, "b", lo = lo_b, hi = hi_b, loaded = d$b)

  analytic <- SIEVER:::sieve_spec_table(d, r$data, r$meta, values = "processed")
  loaded <- SIEVER:::sieve_spec_table(d, r$data, r$meta, values = "raw")
  # type_source (where each type came from) follows type.
  expect_identical(
    names(analytic),
    c(
      "siever_specification", "data_values", "variable", "role", "kind", "label", "wording",
      "type", "type_source", "construct", "facet", "status", "reverse", "reverse_applied",
      "rev_lo", "rev_hi", "rev_bounds", "loaded_reversed", "key_processed", "key_loaded"
    )
  )
  # A file whose kind is analytic is read as processed; both names are
  # read, and processed is written.
  expect_identical(SIEVER:::sieve_export_kinds(), c("raw", "processed"))
  expect_identical(SIEVER:::sieve_export_kind("analytic"), "processed")
  expect_identical(SIEVER:::sieve_spec_table(d, r$data, r$meta, values = "analytic"), analytic)
  expect_true(all(vapply(analytic, is.character, logical(1))))
  expect_identical(analytic$variable, c("id", "a", "b", "c"))
  expect_identical(analytic$role, c("identifier", "item", "item", "item"))
  expect_identical(analytic$siever_specification, rep("2", 4))
  expect_identical(analytic$status, c("", "included", "included", "excluded"))
  expect_identical(analytic$construct, c("", "Alpha", "Alpha", "Beta"))
  expect_identical(analytic$facet, c("", "one", "", ""))
  expect_identical(analytic$reverse, c("", "TRUE", "TRUE", "FALSE"))
  expect_identical(analytic$rev_bounds, c("", "declared", "declared", ""))
  # What each kind of data file holds: the analytic file has the reversals,
  # the file as loaded does not, and each specification says so.
  expect_identical(analytic$data_values, rep("processed", 4))
  expect_identical(analytic$reverse_applied, c("", "TRUE", "TRUE", "FALSE"))
  expect_identical(loaded$data_values, rep("raw", 4))
  expect_identical(loaded$reverse_applied, c("", "FALSE", "FALSE", "FALSE"))
  expect_identical(loaded$loaded_reversed, c("", "FALSE", "FALSE", "FALSE"))
  expect_identical(
    loaded[setdiff(names(loaded), c("data_values", "reverse_applied"))],
    analytic[setdiff(names(analytic), c("data_values", "reverse_applied"))]
  )
  # The keys are those of the values written out by hand.
  expect_identical(analytic$key_processed[2], SIEVER:::sieve_values_key(c(7, 6, 1, NA, 4, 2)))
  expect_identical(analytic$key_loaded[2], SIEVER:::sieve_values_key(c(1, 2, 7, NA, 4, 6)))
  expect_identical(analytic$key_processed[4], analytic$key_loaded[4]) # never reversed

  # The bounds survive the file exactly: 1 and 7 as plain decimals, the
  # others in a form that reads back to the same double.
  expect_identical(analytic$rev_lo[2:3], c("1", sprintf("%a", lo_b)))
  expect_identical(analytic$rev_hi[2], "7")
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  SIEVER:::sieve_spec_write(analytic, path)
  read <- SIEVER:::sieve_spec_read(path)
  expect_identical(as.list(read), as.list(analytic))
  expect_identical(as.numeric(read$rev_lo[3]), lo_b)
  expect_identical(as.numeric(read$rev_hi[3]), hi_b)
  # 15 significant digits, which is what write.csv would have written, do not.
  expect_false(identical(as.numeric(sprintf("%.15g", lo_b)), lo_b))

  # The data frames that go with the two specifications.
  out_a <- SIEVER:::sieve_export_data(d, r$data, r$meta, values = "processed")
  out_l <- SIEVER:::sieve_export_data(d, r$data, r$meta, values = "raw")
  expect_identical(attr(out_a, "siever_values"), "processed")
  expect_identical(attr(out_l, "siever_values"), "raw")
  expect_identical(as.numeric(out_a$a), c(7, 6, 1, NA, 4, 2))
  expect_identical(as.numeric(out_l$a), as.numeric(d$a))
  expect_identical(as.numeric(out_l$b), d$b)
  expect_identical(out_a$id, d$id)
  # The file states its own keying in the variable label.
  expect_identical(attr(out_a$a, "label"), "a [reverse scored within 1 to 7]")
  expect_identical(attr(out_l$a, "label"), "a")
  expect_identical(attr(out_a$c, "label"), "c")
})

# One session that reverse scores two items (one with a missing response
# and whole number bounds, one with non-integer values and observed
# bounds), excludes an item, moves a criterion out of the pool, names
# constructs and a facet, and downloads both kinds of data file with their
# specifications.
ei_exports <- function() {
  skip_if_not_installed("haven")
  d <- ei_survey()
  path <- ei_csv(d)
  st <- sieve_new_state()
  out <- NULL
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, path)
    session$setInputs(cell_edit = list(item = "perf", field = "role", value = "Criterion"))
    for (it in paste0("q", 1:4)) {
      session$setInputs(cell_edit = list(item = it, field = "construct", value = "Engagement"))
    }
    for (it in paste0("q", 5:7)) {
      session$setInputs(cell_edit = list(item = it, field = "construct", value = "Strain"))
    }
    session$setInputs(cell_edit = list(item = "q1", field = "label", value = "Looks forward to work"))
    session$setInputs(cell_edit = list(item = "q5", field = "type", value = "continuous"))
    session$setInputs(cell_edit = list(item = "q6", field = "status", value = "Dropped"))
    shiny::isolate(state$meta$facet[state$meta$item %in% c("q1", "q2")] <- "vigor")
    SIEVER:::sieve_apply_reverse(state, "q2", lo = 1, hi = 5, origin = "ai")
    session$setInputs(cell_edit = list(item = "q7", field = "reverse", value = "Yes"))
    out <<- list(
      csv = path, data = d,
      raw = state$raw, items = state$items, meta = state$meta, roles = state$roles,
      id_vars = sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), revision = SIEVER:::sieve_revision(state)
    )
  })
  processed <- ei_export(st, "processed")
  loaded <- ei_export(st, "raw")
  out$sav <- c(processed = processed$data, raw = loaded$data)
  out$spec <- c(processed = processed$spec, raw = loaded$spec)
  out
}

# A fresh session that loads a data file and restores a specification.
ei_reimport <- function(data_file, spec_file, keying = NULL, name = basename(data_file)) {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, data_file, name = name)
    if (!is.null(keying)) {
      session$setInputs(spec_keying = keying)
    }
    session$setInputs(up_spec = list(name = "specification.csv", datapath = spec_file))
  })
  st
}

# What a round trip has to give back, compared with the exporting session.
expect_same_project <- function(st, ex, info) {
  now <- ei_whole(st)
  expect_identical(lapply(now$items, as.numeric), lapply(ex$items, as.numeric), info = info)
  expect_identical(now$meta[ei_ledger_cols], ex$meta[ei_ledger_cols], info = info)
  expect_identical(now$roles[c("variable", "role")], ex$roles[c("variable", "role")], info = info)
  expect_identical(now$id_vars, ex$id_vars, info = info)
  expect_identical(SIEVER:::sieve_data_key(now$items), ex$revision$data_key, info = info)
  expect_identical(SIEVER:::sieve_meta_key(now$meta), ex$revision$meta_key, info = info)
}

test_that("export and reimport preserve values, status, bounds and direction for a processed and for a raw export", {
  ex <- ei_exports()
  d <- ex$data
  # What the exporting session held, worked out by hand from the file.
  expect_identical(ex$items$q2, (1 + 5) - as.numeric(d$q2))
  expect_identical(ex$meta$status[ex$meta$item == "q6"], "parked")
  expect_identical(ex$meta$reverse, ex$meta$item %in% c("q2", "q7"))
  lo7 <- ex$meta$rev_lo[ex$meta$item == "q7"]
  hi7 <- ex$meta$rev_hi[ex$meta$item == "q7"]
  expect_identical(c(lo7, hi7), range(ex$raw$q7))

  # The data files hold what their names say.
  analytic <- as.data.frame(haven::read_sav(ex$sav[["processed"]]))
  loaded <- as.data.frame(haven::read_sav(ex$sav[["raw"]]))
  expect_identical(as.numeric(analytic$q2), ex$items$q2)
  expect_identical(as.numeric(loaded$q2), as.numeric(ex$raw$q2))
  expect_identical(as.numeric(loaded$q7), as.numeric(ex$raw$q7))
  expect_identical(attr(analytic$q2, "label"), "q2 [reverse scored within 1 to 5]")
  expect_identical(attr(loaded$q2, "label"), "q2")
  for (v in c("id", "sex", "gpa", "perf")) {
    expect_identical(as.numeric(analytic[[v]]), as.numeric(ex$raw[[v]]), info = v)
  }

  # The specification has every item's status, and says what its data file holds.
  spec <- utils::read.csv(ex$spec[["processed"]], colClasses = "character")
  expect_true(all(c("status", "role", "rev_lo", "rev_hi", "reverse_applied", "data_values") %in% names(spec)))
  expect_identical(spec$status[spec$variable == "q6"], "excluded")
  expect_identical(unique(spec$data_values), "processed")
  expect_identical(spec$reverse_applied[spec$variable %in% c("q2", "q7")], c("TRUE", "TRUE"))
  spec_l <- utils::read.csv(ex$spec[["raw"]], colClasses = "character")
  expect_identical(unique(spec_l$data_values), "raw")
  expect_identical(spec_l$reverse_applied[spec_l$variable %in% c("q2", "q7")], c("FALSE", "FALSE"))

  # Analytic data with its specification: the reversal is recorded, never repeated.
  a <- ei_reimport(ex$sav[["processed"]], ex$spec[["processed"]])
  expect_same_project(a, ex, "processed")
  kinds <- vapply(shiny::isolate(a$transforms), function(r) r$type, character(1))
  expect_identical(unname(kinds), c("keying", "keying"))
  expect_identical(
    lapply(shiny::isolate(a$items), as.numeric),
    lapply(shiny::isolate(a$raw)[names(shiny::isolate(a$items))], as.numeric)
  )
  expect_identical(shiny::isolate(a$meta$label[a$meta$item == "q2"]), "q2") # not the file's suffix
  expect_false(isTRUE(shiny::isolate(a$confirmed)))

  # Raw data with its specification: the reversal is applied once, within
  # the bounds of the first session, and gives the very same values.
  b <- ei_reimport(ex$sav[["raw"]], ex$spec[["raw"]])
  expect_same_project(b, ex, "as loaded")
  recs <- shiny::isolate(b$transforms)
  expect_identical(
    unname(vapply(recs, function(r) paste(r$type, r$item, r$origin), character(1))),
    c("reverse q2 specification", "reverse q7 specification")
  )
  expect_identical(unname(vapply(recs, function(r) r$bounds, character(1))), c("declared", "observed"))
  expect_identical(c(recs[[2]]$lo, recs[[2]]$hi), c(lo7, hi7))
  expect_identical(
    lapply(shiny::isolate(b$raw)[c("q2", "q7")], as.numeric),
    lapply(ex$raw[c("q2", "q7")], as.numeric)
  )

  # The two files crossed: each specification knows both kinds of values.
  expect_same_project(ei_reimport(ex$sav[["raw"]], ex$spec[["processed"]]), ex, "raw data, processed specification")
  crossed <- ei_reimport(ex$sav[["processed"]], ex$spec[["raw"]])
  expect_same_project(crossed, ex, "processed data, raw specification")
  expect_true(all(vapply(shiny::isolate(crossed$transforms), function(r) r$type, character(1)) == "keying"))
  # The file first loaded, with either specification.
  expect_same_project(ei_reimport(ex$csv, ex$spec[["processed"]]), ex, "first file, processed specification")
  expect_same_project(ei_reimport(ex$csv, ex$spec[["raw"]]), ex, "first file, raw specification")
})

test_that("restoring a specification twice changes nothing the second time, and a reversal can still be taken back", {
  ex <- ei_exports()
  for (kind in c("processed", "raw")) {
    st <- ei_reimport(ex$sav[[kind]], ex$spec[[kind]])
    shiny::testServer(mod_ingest_server, args = list(state = st), {
      once <- ei_whole(state)
      for (again in c("processed", "raw")) {
        session$setInputs(up_spec = list(name = paste0(again, ".csv"), datapath = ex$spec[[again]]))
        expect_identical(state$items, once$items, info = paste(kind, again))
        expect_identical(state$meta, once$meta, info = paste(kind, again))
        expect_identical(state$transforms, once$transforms, info = paste(kind, again))
      }
      # Taking the reversal of the whole number item back gives the values
      # as administered, exactly.
      session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "No"))
      expect_identical(as.numeric(state$items$q2), as.numeric(ex$data$q2), info = kind)
      expect_false(state$meta$reverse[state$meta$item == "q2"])
      # For the non-integer item the raw file returns the loaded values
      # themselves. The analytic file never held them: reflecting its
      # values again agrees to rounding (see the numerical check of this
      # stage), and that is all an analytic file can give.
      session$setInputs(cell_edit = list(item = "q7", field = "reverse", value = "No"))
      if (identical(kind, "raw")) {
        expect_identical(as.numeric(state$items$q7), as.numeric(ex$raw$q7))
      } else {
        expect_equal(as.numeric(state$items$q7), as.numeric(ex$raw$q7), tolerance = 1e-14)
      }
    })
  }
})

test_that("transformed data and their specification are not reversed again", {
  skip_if_not_installed("haven")
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  shiny::isolate({
    st$dataset_name <- fx$name
    st$raw <- fx$data
    st$items <- fx$data[, fx$item_cols, drop = FALSE]
    st$id_vars <- fx$id_vars
    st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    st$confirmed <- TRUE
  })
  held <- new.env(parent = emptyenv())
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    held$it <- state$meta$item[1]
    held$original <- state$items[[held$it]]
    session$setInputs(cell_edit = list(item = held$it, field = "reverse", value = "Yes"))
    held$transformed <- state$items[[held$it]]
    expect_identical(range(held$original), c(4L, 51L))
    expect_identical(held$transformed, (4 + 51) - as.numeric(held$original)) # its observed range
  })
  files <- ei_export(st, "processed")
  spec <- utils::read.csv(files$spec)
  expect_true("status" %in% names(spec))
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    it <- held$it
    session$setInputs(source = "upload", file = list(name = "export.sav", datapath = files$data), load = 1L)
    # The state holds a study already, so Load asks first.
    session$setInputs(load_replace = 1L)
    session$setInputs(up_spec = list(name = "spec.csv", datapath = files$spec))
    expect_identical(as.numeric(state$items[[it]]), as.numeric(held$transformed))
    expect_false(isTRUE(all.equal(as.numeric(state$items[[it]]), as.numeric(held$original))))
    expect_true(state$meta$reverse[match(it, state$meta$item)])
    expect_identical(c(state$meta$rev_lo[1], state$meta$rev_hi[1]), c(4, 51))
    expect_length(Filter(
      function(r) identical(r$type, "reverse") && identical(r$dataset_id, state$dataset_id),
      state$transforms
    ), 0L)
  })
})

test_that("when the loaded values are not the saved ones the import asks instead of guessing", {
  ex <- ei_exports()
  # Other data on the same instrument: the first forty rows of the raw file.
  other <- ex$data[1:40, ]
  other_csv <- ei_csv(other, "other_sample.csv")

  refused <- ei_reimport(other_csv, ex$spec[["processed"]])
  fresh <- ei_state(other)
  expect_identical(
    ei_whole(refused)[c("items", "meta", "roles", "id_vars", "transforms", "raw")],
    ei_whole(fresh)[c("items", "meta", "roles", "id_vars", "transforms", "raw")]
  )
  out <- shiny::isolate(SIEVER:::sieve_apply_spec(refused, ex$spec[["processed"]]))
  expect_false(out$ok)
  # Every item with keys is named: the two reverse keyed ones, and the ones
  # the specification lists as not reverse keyed, whose values it cannot
  # vouch for either.
  expect_match(
    paste(out$problems, collapse = "\n"),
    "marks q2, q7 as reverse keyed, but the values now loaded for them are not"
  )
  expect_match(
    paste(out$problems, collapse = "\n"),
    "lists q1, q3, q4, q5, q6 as not reverse keyed, but the values now loaded for them are not"
  )

  # The researcher says the data hold the items as administered: they are
  # reverse scored once, within the bounds of the specification.
  administered <- ei_reimport(other_csv, ex$spec[["processed"]], keying = "administered")
  now <- ei_whole(administered)
  expect_identical(now$items$q2, (1 + 5) - as.numeric(other$q2))
  lo7 <- ex$meta$rev_lo[ex$meta$item == "q7"]
  hi7 <- ex$meta$rev_hi[ex$meta$item == "q7"]
  expect_identical(now$items$q7, (lo7 + hi7) - as.numeric(now$raw$q7))
  expect_identical(now$meta$reverse, now$meta$item %in% c("q2", "q7"))
  expect_identical(now$meta$status[now$meta$item == "q6"], "parked")
  expect_identical(now$id_vars, ex$id_vars)

  # Or that they are already reverse scored: recorded, and not one value changed.
  saved <- ei_reimport(other_csv, ex$spec[["raw"]], keying = "reversed")
  now <- ei_whole(saved)
  expect_identical(lapply(now$items, as.numeric), lapply(now$raw[names(now$items)], as.numeric))
  expect_identical(now$meta$reverse, now$meta$item %in% c("q2", "q7"))
  expect_identical(now$meta$rev_lo[now$meta$item == "q2"], 1)
  expect_true(all(vapply(now$transforms, function(r) r$type, character(1)) == "keying"))

  # The other direction: the saved data held an item reverse scored and the
  # session had returned it. For other data that cannot be told either.
  held <- utils::read.csv(ex$spec[["raw"]], colClasses = "character")
  at <- match("q3", held$variable)
  held$loaded_reversed[at] <- "TRUE"
  held$key_loaded[at] <- strrep("0", 32)
  held$key_processed[at] <- strrep("1", 32)
  held <- held[held$variable %in% c("q1", "q3"), ]
  held_path <- tempfile(fileext = ".csv")
  utils::write.csv(held, held_path, row.names = FALSE)
  ask <- shiny::isolate(SIEVER:::sieve_apply_spec(ei_state(other), held_path))
  expect_false(ask$ok)
  expect_match(
    paste(ask$problems, collapse = "\n"),
    "held q3 reverse scored, and the values now loaded for it are not"
  )

  # A specification can declare response bounds without saying anything
  # about the keying: the bounds are stored, nothing is reflected, and the
  # next reversal of the item is made within them, not the observed range.
  bounds_only <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(variable = "q7", rev_lo = 0, rev_hi = 2), bounds_only, row.names = FALSE)
  declared <- ei_reimport(ex$csv, bounds_only)
  shiny::testServer(mod_ingest_server, args = list(state = declared), {
    i <- match("q7", state$meta$item)
    expect_identical(state$meta$reverse[i], FALSE)
    expect_identical(c(state$meta$rev_lo[i], state$meta$rev_hi[i]), c(0, 2))
    expect_identical(state$meta$rev_bounds[i], "declared")
    expect_identical(state$items$q7, state$raw$q7)
    expect_length(state$transforms, 0L)
    session$setInputs(cell_edit = list(item = "q7", field = "reverse", value = "Yes"))
    expect_identical(state$items$q7, (0 + 2) - as.numeric(state$raw$q7))
    expect_identical(state$transforms[[1]]$bounds, "declared")
  })

  # A specification written by hand, without keys, is taken at its word
  # when it says what the data hold, and refused when it does not.
  by_hand <- function(df) {
    path <- tempfile(fileext = ".csv")
    utils::write.csv(df, path, row.names = FALSE)
    path
  }
  says <- by_hand(data.frame(
    variable = c("q3", "q4"), reverse = c(TRUE, TRUE),
    reverse_applied = c(TRUE, FALSE), rev_lo = 1, rev_hi = 5
  ))
  hand <- ei_whole(ei_reimport(ex$csv, says))
  expect_identical(as.numeric(hand$items$q3), as.numeric(ex$data$q3)) # already held it
  expect_identical(hand$items$q4, (1 + 5) - as.numeric(ex$data$q4)) # reversed now
  expect_identical(hand$meta$reverse[hand$meta$item %in% c("q3", "q4")], c(TRUE, TRUE))
  silent <- by_hand(data.frame(item = c("q3", "q4"), reverse = c(TRUE, FALSE)))
  quiet <- ei_reimport(ex$csv, silent)
  expect_identical(ei_whole(quiet)$meta$reverse, rep(FALSE, 8))
  expect_match(
    shiny::isolate(SIEVER:::sieve_apply_spec(quiet, silent))$problems,
    "does not say whether the data file already holds the reversal"
  )
})

test_that("an import never overwrites the bounds of a reversal in force", {
  ex <- ei_exports()
  st <- ei_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "perf", field = "role", value = "Criterion"))
    # Here q2 is reverse scored within 0 to 6, not the 1 to 5 of the specification.
    SIEVER:::sieve_apply_reverse(state, "q2", lo = 0, hi = 6, origin = "ai")
    held <- state$items$q2
    out <- SIEVER:::sieve_apply_spec(state, ex$spec[["processed"]])
    expect_true(out$ok)
    expect_identical(out$summary$bounds_kept, "q2")
    expect_match(out$message, "bounds in the specification were not used for q2")
    expect_identical(state$items$q2, held)
    expect_identical(c(
      state$meta$rev_lo[state$meta$item == "q2"],
      state$meta$rev_hi[state$meta$item == "q2"]
    ), c(0, 6))
    # And the invariant of the service holds: the item is the single
    # reflection of its loaded values within the bounds in the ledger.
    expect_identical(state$items$q2, (0 + 6) - as.numeric(state$raw$q2))
  })
})

test_that("an invalid specification is rejected with a specific message and leaves the project unchanged", {
  ex <- ei_exports()
  good <- utils::read.csv(ex$spec[["raw"]], colClasses = "character", check.names = FALSE)
  row <- function(v) match(v, good$variable)
  edit <- function(change) {
    spec <- change(good)
    path <- tempfile(fileext = ".csv")
    utils::write.csv(spec, path, row.names = FALSE, na = "")
    path
  }
  cases <- list(
    "needs a column named variable" = edit(function(s) s[setdiff(names(s), "variable")]),
    "q3 is listed 2 times, under the constructs Engagement, Strain" = edit(function(s) {
      s <- rbind(s, s[row("q3"), ])
      s$construct[nrow(s)] <- "Strain"
      s
    }),
    "no variable name \\(line 3" = edit(function(s) {
      s$variable[2] <- ""
      s
    }),
    "A measurement type is not one of continuous, ordered, binary: q1 \\(\"likert\"\\)" = edit(function(s) {
      s$type[row("q1")] <- "likert"
      s
    }),
    "A status is neither included nor excluded: q4 \\(\"maybe\"\\)" = edit(function(s) {
      s$status[row("q4")] <- "maybe"
      s
    }),
    "1 variable in the specification is not in the loaded data: q99" = edit(function(s) {
      s$variable[row("q5")] <- "q99"
      s
    }),
    "A role is not one of item, identifier, group, criterion, antecedent, auxiliary: sex \\(\"covariate\"\\)" = edit(function(s) {
      s$role[row("sex")] <- "covariate"
      s
    }),
    "site is not stored as a number" = edit(function(s) {
      s$role[row("site")] <- "item"
      s
    }),
    "A construct has the name of a variable in the data \\(construct sex for q1\\)" = edit(function(s) {
      s$construct[row("q1")] <- "sex"
      s
    }),
    "need both a minimum \\(rev_lo\\) and a maximum \\(rev_hi\\): q2" = edit(function(s) {
      s$rev_hi[row("q2")] <- ""
      s
    }),
    "need a minimum below the maximum: q2" = edit(function(s) {
      s$rev_lo[row("q2")] <- "5"
      s
    }),
    "A rev_lo cell is not a number: q2 \\(\"low\"\\)" = edit(function(s) {
      s$rev_lo[row("q2")] <- "low"
      s
    }),
    "A reverse cell is neither TRUE nor FALSE: q3 \\(\"sometimes\"\\)" = edit(function(s) {
      s$reverse[row("q3")] <- "sometimes"
      s
    }),
    "A rev_bounds cell is neither declared nor observed" = edit(function(s) {
      s$rev_bounds[row("q2")] <- "guessed"
      s
    }),
    "A key_loaded cell is not a content key" = edit(function(s) {
      s$key_loaded[row("q2")] <- "abc"
      s
    }),
    "written by a newer SIEVER \\(specification schema 3" = edit(function(s) {
      s$siever_specification <- "3"
      s
    }),
    "A variable that is not an item cannot be reverse scored: sex" = edit(function(s) {
      s$reverse[row("sex")] <- "TRUE"
      s
    }),
    "A facet needs a construct to belong to, and these items have a facet and no construct: q5" = edit(function(s) {
      s$construct[row("q5")] <- ""
      s$facet[row("q5")] <- "pace"
      s
    }),
    # A valid change (the construct of q1) and, further down, a reversal
    # that cannot be made (values outside the bounds given): nothing of it
    # is applied, the valid part included.
    "values of item \"q3\" lie outside the response bounds 2 to 4" = edit(function(s) {
      s$construct[row("q1")] <- "Renamed"
      s$reverse[row("q3")] <- "TRUE"
      s$rev_lo[row("q3")] <- "2"
      s$rev_hi[row("q3")] <- "4"
      s$key_processed[row("q3")] <- ""
      s$key_loaded[row("q3")] <- ""
      s$reverse_applied[row("q3")] <- "FALSE"
      s
    }),
    "The specification has no rows" = {
      path <- tempfile(fileext = ".csv")
      writeLines("variable,role,type", path)
      path
    },
    "could not be read as a comma separated table" = {
      path <- tempfile(fileext = ".csv")
      file.create(path)
      path
    },
    "needs a column named variable|could not be read" = ex$sav[["raw"]]
  )

  st <- ei_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    # A project with work in it, so that an import has something to disturb.
    session$setInputs(cell_edit = list(item = "perf", field = "role", value = "Criterion"))
    session$setInputs(cell_edit = list(item = "q4", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "q1", field = "construct", value = "Mine"))
    session$setInputs(confirm = 1L)
    whole <- ei_whole(state)
    revision <- SIEVER:::sieve_revision(state)
    for (i in seq_along(cases)) {
      what <- names(cases)[i]
      session$setInputs(up_spec = list(name = sprintf("case_%d.csv", i), datapath = cases[[i]]))
      expect_identical(ei_whole(state), whole, info = what)
      expect_identical(SIEVER:::sieve_revision(state), revision, info = what)
      out <- SIEVER:::sieve_apply_spec(state, cases[[i]])
      expect_false(out$ok, info = what)
      expect_match(paste(out$problems, collapse = "\n"), what, info = what)
      expect_identical(ei_whole(state), whole, info = what)
    }
    # Several problems in one file are all reported.
    many <- edit(function(s) {
      s$type[row("q1")] <- "likert"
      s$status[row("q4")] <- "maybe"
      s$variable[row("q5")] <- "q99"
      s
    })
    out <- SIEVER:::sieve_apply_spec(state, many)
    expect_length(out$problems, 3L)
    expect_identical(ei_whole(state), whole)
    # The valid specification is then applied in full: the same session can
    # tell a refusal from an import that works.
    session$setInputs(up_spec = list(name = "good.csv", datapath = ex$spec[["raw"]]))
    expect_identical(state$meta$construct[state$meta$item == "q1"], "Engagement")
    expect_false(isTRUE(state$confirmed))
    expect_false(identical(SIEVER:::sieve_revision(state), revision))
  })
})

# ---- The project snapshot ---------------------------------------------------

# Anything in an object that is not plain data, found without the package's
# own walker.
ei_live_parts <- function(x, path = "x") {
  if (is.environment(x) || is.function(x) || isS4(x) || is.language(x) ||
    typeof(x) %in% c("externalptr", "weakref", "promise", "bytecode")) {
    return(path)
  }
  out <- character(0)
  for (name in names(attributes(x))) {
    if (!name %in% c("names", "class", "row.names", "levels", "dim", "dimnames")) {
      out <- c(out, ei_live_parts(attr(x, name), paste0(path, "@", name)))
    }
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      out <- c(out, ei_live_parts(x[[i]], paste0(path, "/", names(x)[i] %||% i)))
    }
  }
  out
}

# A session with work of every kind in it: the bundled example loaded by
# the real observer, an item reverse scored within declared non-integer
# bounds, one excluded, a role changed, and a fitted model in the registry.
ei_project_state <- function() {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    ei_quiet(session$setInputs(load = 1L))
    SIEVER:::sieve_apply_reverse(state, "t2_cubes", lo = 0.5, hi = 40.25, origin = "ai")
    session$setInputs(cell_edit = list(
      item = "t13_straight_and_curved_capitals", field = "status",
      value = "Dropped"
    ))
    session$setInputs(cell_edit = list(item = "age_years", field = "role", value = "Criterion"))
    session$setInputs(cell_edit = list(item = "t9_word_meaning", field = "reverse", value = "Yes"))
    session$setInputs(confirm = 1L)
  })
  shiny::isolate({
    spec <- sieve_cfa_spec(st$meta)
    result <- sieve_run_cfa(spec, st$items)
    sieve_register_analysis(st, result, note = "Confirmatory model before saving")
  })
  st
}

test_that("a snapshot is a plain list that holds the whole project and no fitted object", {
  st <- ei_project_state()
  shiny::isolate({
    # A fitted engine object put into a registry record, as a later change
    # to the registry might: the snapshot must leave it out and say so.
    fit <- lavaan::cfa("f =~ x1 + x2 + x3", lavaan::HolzingerSwineford1939)
    held <- st$analyses
    held$A1$fit <- fit
    st$analyses <- held
  })
  snap <- SIEVER:::sieve_project_snapshot(st)
  now <- ei_whole(st)

  expect_s3_class(snap, "sieve_project")
  expect_identical(typeof(snap), "list")
  expect_false(shiny::is.reactivevalues(snap))
  expect_length(ei_live_parts(unclass(snap)), 0L)
  expect_identical(snap$dropped, "analyses$A1$fit")
  expect_false("fit" %in% names(snap$analyses$A1))
  expect_null(snap$analyses$A1[["fit", exact = TRUE]])
  expect_identical(
    snap$analyses$A1[setdiff(names(snap$analyses$A1), "fit")],
    now$analyses$A1[setdiff(names(now$analyses$A1), "fit")]
  )
  # The status the analysis had is kept as it was, whatever it was.
  expect_identical(snap$analyses$A1$status, now$analyses$A1$status)
  expect_true(snap$analyses$A1$status$code %in%
    c("ok", "inadmissible", "not_identified", "not_converged", "failed"))

  expect_identical(snap$format, "SIEVER project")
  expect_identical(snap$schema_version, 3L)
  expect_identical(snap$held_rows, list())
  expect_identical(snap$keys$content, SIEVER:::sieve_content_key(snap))
  expect_identical(snap$siever_version, SIEVER:::sieve_version())
  expect_true(all(c(
    "raw", "items", "meta", "theory", "roles", "confirmed", "estimator",
    "thresholds", "transforms", "analyses", "codelog", "revision", "keys",
    "construct_basis", "dataset", "saved"
  ) %in% names(snap)))
  expect_identical(snap$raw, now$raw)
  expect_identical(snap$items, now$items)
  expect_identical(snap$meta, now$meta)
  expect_identical(snap$theory, now$theory)
  expect_identical(snap$roles, now$roles)
  expect_identical(snap$id_vars, snap$roles$variable[snap$roles$role != "item"])
  expect_true(snap$confirmed)
  expect_identical(snap$estimator, now$estimator)
  expect_identical(snap$thresholds, now$thresholds)
  expect_identical(snap$transforms, now$transforms)
  expect_identical(snap$codelog, now$codelog)
  expect_identical(snap$revision, shiny::isolate(SIEVER:::sieve_revision(st)))
  expect_identical(snap$dataset$id, "D1")
  expect_length(snap$transforms, 2L)
  expect_identical(SIEVER:::sieve_project_check(snap), character(0))

  # A state assembled without a roles table still gives a complete snapshot.
  bare <- sieve_new_state()
  fx <- sieve_hs9_fixture()
  shiny::isolate({
    bare$raw <- fx$data
    bare$items <- fx$data[, fx$item_cols, drop = FALSE]
    bare$meta <- sieve_build_meta(fx$data, fx$item_cols)
    bare$id_vars <- fx$id_vars
  })
  s2 <- SIEVER:::sieve_project_snapshot(bare)
  expect_identical(s2$roles$variable[s2$roles$role == "item"], fx$item_cols)
  expect_identical(s2$roles$variable[s2$roles$role != "item"], fx$id_vars)
  expect_identical(SIEVER:::sieve_project_check(s2), character(0))
  expect_error(SIEVER:::sieve_project_snapshot(sieve_new_state()), "No data are loaded",
    class = "sieve_project_error"
  )
})

test_that("snapshot, write, read and restore give the project back exactly, under a new dataset identity", {
  st <- ei_project_state()
  saved <- ei_whole(st)
  saved_revision <- shiny::isolate(SIEVER:::sieve_revision(st))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  SIEVER:::sieve_project_write(SIEVER:::sieve_project_snapshot(st), path)
  # Only the project file is left beside the target.
  expect_length(list.files(dirname(path), pattern = "^siever_project_.*\\.part$"), 0L)
  snap <- SIEVER:::sieve_project_read(path)

  # Into a session that has other data, an undo record and a kept result.
  other <- ei_state()
  shiny::isolate(SIEVER:::sieve_apply_reverse(other, "q3", origin = "ai"))
  old_record <- shiny::isolate(other$transforms$T1)
  old_revision <- shiny::isolate(SIEVER:::sieve_revision(other))
  out <- SIEVER:::sieve_project_restore(other, snap, source = "saved.rds")
  now <- ei_whole(other)

  # The data keys, the ledger, the transformations, the roles and the registry, exactly.
  expect_identical(SIEVER:::sieve_data_key(now$raw), SIEVER:::sieve_data_key(saved$raw))
  expect_identical(SIEVER:::sieve_data_key(now$items), saved_revision$data_key)
  expect_identical(now$raw, saved$raw)
  expect_identical(now$items, saved$items)
  expect_identical(now$meta, saved$meta)
  # The records of the saved dataset (D1) belong to the data now loaded (D2):
  # everything else about them is as saved.
  expect_identical(now$transforms, ei_relabel(saved$transforms, "D1", "D2"))
  expect_identical(now$roles, saved$roles)
  expect_identical(now$id_vars, saved$id_vars)
  expect_identical(now$analyses, ei_relabel(saved$analyses, "D1", "D2"))
  expect_identical(now$analyses$A1$revision$dataset_id, "D2")
  expect_identical(now$codelog, saved$codelog)
  expect_identical(now$theory, saved$theory)
  expect_identical(now$confirmed, saved$confirmed)
  expect_identical(now$estimator, saved$estimator)
  expect_identical(now$thresholds, saved$thresholds)
  expect_identical(now$dataset_name, saved$dataset_name)

  # A new dataset identity, used by neither history.
  revision <- shiny::isolate(SIEVER:::sieve_revision(other))
  expect_identical(out$dataset_id, now$dataset_id)
  expect_identical(out$saved_dataset_id, "D1")
  used <- c(vapply(saved$transforms, function(r) r$dataset_id, character(1)), "D1", old_record$dataset_id)
  expect_false(now$dataset_id %in% used)
  expect_identical(now$dataset_id, "D2")
  expect_true(SIEVER:::sieve_same_revision(saved_revision, revision, what = c("data", "meta")))
  expect_false(SIEVER:::sieve_same_revision(saved_revision, revision, what = "dataset"))
  expect_identical(
    now$project[c(
      "source", "schema_version", "saved_dataset_id", "dataset_id",
      "container_version"
    )],
    list(
      source = "saved.rds", schema_version = 3L, saved_dataset_id = "D1",
      dataset_id = "D2", container_version = 1L
    )
  )
  # Nothing kept from before the restore applies to the restored data.
  expect_match(SIEVER:::sieve_stale_reason(old_revision, revision), "different dataset")
  # The saved analysis and the saved reversal are current for the restored
  # data, which are the data they were made on, not reported as "a different
  # dataset" after reopening.
  expect_null(SIEVER:::sieve_stale_reason(now$analyses$A1$revision, revision))

  # The restored project works: the reversal made before saving is taken
  # back to the loaded values exactly, and a second snapshot is the first.
  again <- SIEVER:::sieve_project_snapshot(other)
  first <- SIEVER:::sieve_project_snapshot(st)
  same <- setdiff(names(first), c("saved", "dataset", "revision", "transforms", "analyses", "keys"))
  expect_identical(again$other$confirmation$dataset_id, "D2")
  expect_identical(first$other$confirmation$dataset_id, "D1")
  first$other$confirmation$dataset_id <- "D2"
  expect_identical(unclass(again)[same], unclass(first)[same])
  expect_identical(again$transforms, ei_relabel(first$transforms, "D1", "D2"))
  expect_identical(again$analyses, ei_relabel(first$analyses, "D1", "D2"))
  expect_identical(
    again$keys[c("raw", "items", "meta", "ledger", "roles")],
    first$keys[c("raw", "items", "meta", "ledger", "roles")]
  )
  shiny::isolate(SIEVER:::sieve_apply_reverse(other, "t2_cubes"))
  expect_identical(shiny::isolate(other$items$t2_cubes), saved$raw$t2_cubes)
  expect_identical(shiny::isolate(other$transforms[[3]]$dataset_id), "D2")
})

test_that("Save project (Reproducible Code tab) and Open project (Ingest tab) work, and a bad file leaves the session unchanged", {
  st <- ei_project_state()
  saved <- ei_whole(st)
  path <- NULL
  # Save project lives on the Reproducible Code tab's Export panel; it
  # writes the same project file twice over.
  shiny::testServer(mod_repro_server, args = list(state = st), {
    first <- output$save_project
    again <- output$save_project
    expect_true(file.exists(first) && file.exists(again))
    a <- SIEVER:::sieve_project_read(first)
    b <- SIEVER:::sieve_project_read(again)
    expect_identical(unclass(a)[setdiff(names(a), "saved")], unclass(b)[setdiff(names(b), "saved")])
    expect_identical(attr(a, "container"), 1L)
    path <<- tempfile(fileext = ".rds")
    file.copy(again, path)
  })

  # Files that must be refused, in the project file form SIEVER writes and
  # as a plain RDS, which SIEVER also reads (the two helpers below).
  good <- unclass(SIEVER:::sieve_project_read(path))
  attr(good, "container") <- NULL
  pack <- function(object) {
    file <- tempfile(fileext = ".rds")
    writeBin(SIEVER:::sieve_project_encode(object), file)
    file
  }
  legacy <- function(object) {
    file <- tempfile(fileext = ".rds")
    saveRDS(object, file)
    file
  }
  truncated <- tempfile(fileext = ".rds")
  bytes <- readBin(path, "raw", file.size(path))
  writeBin(bytes[seq_len(length(bytes) %/% 2L)], truncated)
  flipped <- tempfile(fileext = ".rds")
  damaged <- bytes
  damaged[length(damaged) - 40L] <- xor(damaged[length(damaged) - 40L], as.raw(1L))
  writeBin(damaged, flipped)
  edited <- tempfile(fileext = ".rds")
  writeBin(c(charToRaw(sub("schema 3", "schema three", rawToChar(bytes[1:60]))), bytes[-(1:60)]), edited)
  legacy_truncated <- tempfile(fileext = ".rds")
  legacy_bytes <- readBin(legacy(good), "raw", 1e7)
  writeBin(legacy_bytes[seq_len(length(legacy_bytes) %/% 2L)], legacy_truncated)
  not_rds <- tempfile(fileext = ".rds")
  writeLines("variable,role", not_rds)
  bad <- list(
    "holds [0-9]+ bytes of content where its header says" = truncated,
    "content does not match the digest in its header" = flipped,
    "header is not in the form SIEVER writes" = edited,
    "could not be read as a SIEVER project" = legacy_truncated,
    "could not be read as a SIEVER project" = not_rds,
    "not a SIEVER project \\(it is some other R data file\\)" = legacy(datasets::mtcars),
    "saved by a newer SIEVER \\(project schema 4; this version reads up to schema 3\\)" = pack({
      x <- good
      x$schema_version <- 4L
      x
    }),
    "saved by a newer SIEVER \\(project file version 2; this version reads up to version 1\\)" = {
      file <- tempfile(fileext = ".rds")
      writeBin(c(charToRaw(sub("container 1", "container 2", rawToChar(bytes[1:60]))), bytes[-(1:60)]), file)
      file
    },
    # The schema line is outside the digest, so an edited one is caught by
    # comparing it with the content's own version.
    "header names a schema version other than the one its content holds" = {
      file <- tempfile(fileext = ".rds")
      writeBin(c(charToRaw(sub("schema 3", "schema 1", rawToChar(bytes[1:60]))), bytes[-(1:60)]), file)
      file
    },
    "header names no project file version" = {
      file <- tempfile(fileext = ".rds")
      writeBin(c(charToRaw(sub("container 1", "container 0", rawToChar(bytes[1:60]))), bytes[-(1:60)]), file)
      file
    },
    "does not say which schema version" = pack({
      x <- good
      x$schema_version <- NULL
      x
    }),
    "holds something that is not plain data \\(snapshot\\$analyses\\$A1\\$hook: type closure\\), so it was not opened" = pack({
      x <- good
      x$analyses$A1$hook <- function() "run me"
      x
    }),
    "item data are not the ones it was saved with" = pack({
      x <- good
      x$items$t2_cubes[3] <- x$items$t2_cubes[3] + 1
      x
    }),
    "ledger entries are not the ones it was saved with" = pack({
      x <- good
      x$meta$rev_hi[x$meta$item == "t2_cubes"] <- 41
      x
    }),
    "variable roles are not the ones it was saved with" = pack({
      x <- good
      x$roles$role[x$roles$variable == "sex"] <- "auxiliary"
      x$id_vars <- x$roles$variable[x$roles$role != "item"]
      x
    }),
    "records, settings and session script are not the ones it was saved with" = pack({
      x <- good
      at <- grep("holzinger_swineford", x$codelog, fixed = TRUE)[1L]
      x$codelog[at] <- sub("holzinger_swineford", "holzinger_swineferd", x$codelog[at])
      x
    }),
    "roles table and the item data disagree" = legacy({
      x <- good
      x$roles$role[x$roles$variable == "id"] <- "item"
      x
    }),
    "it has no raw" = pack({
      x <- good
      x$raw <- NULL
      x
    })
  )

  other <- ei_state()
  shiny::testServer(mod_ingest_server, args = list(state = other), {
    session$setInputs(cell_edit = list(item = "q4", field = "reverse", value = "Yes"))
    session$setInputs(confirm = 1L)
    whole <- ei_whole(state)
    revision <- SIEVER:::sieve_revision(state)
    session$setInputs(open_project_main = 1L) # the dialog opens without error
    for (i in seq_along(bad)) {
      what <- names(bad)[i]
      session$setInputs(project_file = list(name = sprintf("bad_%d.rds", i), datapath = bad[[i]]))
      # The dialog says why, and pressing Open this project does nothing.
      expect_match(output$project_preview$html, "was not opened, and the session is unchanged", info = what)
      session$setInputs(confirm_open = i)
      expect_identical(ei_whole(state), whole, info = what)
      expect_identical(SIEVER:::sieve_revision(state), revision, info = what)
      expect_error(SIEVER:::sieve_project_read(bad[[i]]), what, class = "sieve_project_error", info = what)
    }
    # The restore function refuses the same objects when they are handed to it directly.
    newer <- good
    newer$schema_version <- 4L
    expect_error(SIEVER:::sieve_project_restore(state, newer), "newer SIEVER", class = "sieve_project_error")
    changed <- good
    changed$items$t2_cubes[3] <- changed$items$t2_cubes[3] + 1
    expect_error(SIEVER:::sieve_project_restore(state, changed), "item data are not the ones",
      class = "sieve_project_error"
    )
    expect_identical(ei_whole(state), whole)

    # The good file: the dialog shows what it holds before anything changes ...
    session$setInputs(project_file = list(name = "saved.rds", datapath = path))
    shown <- output$project_preview$html
    expect_match(shown, "saved.rds holds:")
    expect_match(shown, saved$dataset_name, fixed = TRUE)
    expect_match(shown, "301")
    expect_match(shown, "9 \\(8 included, 1 excluded\\)")
    expect_match(shown, "digest verified")
    expect_match(shown, "Open This Project")
    expect_identical(ei_whole(state), whole)
    # ... and opens through the real observer when the button is pressed.
    session$setInputs(confirm_open = length(bad) + 1L)
    expect_identical(state$items, saved$items)
    expect_identical(state$meta, saved$meta)
    expect_identical(state$roles, saved$roles)
    expect_identical(state$transforms, ei_relabel(saved$transforms, "D1", state$dataset_id))
    expect_identical(state$analyses, ei_relabel(saved$analyses, "D1", state$dataset_id))
    expect_identical(state$codelog, saved$codelog)
    expect_true(isTRUE(state$confirmed))
    expect_false(identical(state$dataset_id, whole$dataset_id))
    expect_identical(state$project$source, "saved.rds")
    expect_identical(state$project$container_version, 1L)
  })

  # An invalid snapshot is not written, and no file is left behind.
  target <- file.path(tempfile("ei_write_"), "project.rds")
  dir.create(dirname(target))
  broken <- good
  broken$items$t2_cubes[1] <- 0
  expect_error(SIEVER:::sieve_project_write(broken, target), "was not saved", class = "sieve_project_error")
  expect_length(list.files(dirname(target)), 0L)
})

test_that("an older project is upgraded one schema version at a time, and a newer one is refused", {
  snap <- list(format = "SIEVER project", schema_version = 1L, trace = character(0))
  steps <- list(
    "1" = function(s) {
      s$trace <- c(s$trace, "one to two")
      s$schema_version <- 2L
      s
    },
    "2" = function(s) {
      s$trace <- c(s$trace, "two to three")
      s$added_in_three <- TRUE
      s$schema_version <- 3L
      s
    }
  )
  up <- SIEVER:::sieve_project_migrate(snap, to = 3L, steps = steps)
  expect_identical(up$schema_version, 3L)
  expect_identical(up$trace, c("one to two", "two to three")) # in order, each once
  expect_identical(up$migrated_from, c(1L, 2L))
  expect_true(up$added_in_three)
  # From the middle: only the remaining step runs.
  mid <- SIEVER:::sieve_project_migrate(utils::modifyList(snap, list(schema_version = 2L)), to = 3L, steps = steps)
  expect_identical(mid$trace, "two to three")
  # Already current: returned as it is.
  expect_identical(SIEVER:::sieve_project_migrate(utils::modifyList(snap, list(schema_version = 3L)),
    to = 3L, steps = steps
  )$trace, character(0))
  # A missing step, a step that does not arrive, a newer schema, and no
  # version at all are refused.
  expect_error(SIEVER:::sieve_project_migrate(snap, to = 3L, steps = steps["1"]),
    "no upgrade from project schema 2 to 3",
    class = "sieve_project_error"
  )
  stuck <- list("1" = function(s) s)
  expect_error(SIEVER:::sieve_project_migrate(snap, to = 2L, steps = stuck),
    "did not produce schema 2",
    class = "sieve_project_error"
  )
  expect_error(
    SIEVER:::sieve_project_migrate(utils::modifyList(snap, list(schema_version = 4L)),
      to = 3L, steps = steps
    ),
    "newer SIEVER \\(project schema 4; this version reads up to schema 3\\)",
    class = "sieve_project_error"
  )
  expect_error(SIEVER:::sieve_project_migrate(list(format = "SIEVER project")),
    "does not say which schema version",
    class = "sieve_project_error"
  )
  expect_error(SIEVER:::sieve_project_migrate(list(schema_version = 1L)), "not a SIEVER project",
    class = "sieve_project_error"
  )
  # SIEVER writes schema 3 and migrates schema 1 and schema 2 files.
  expect_identical(SIEVER:::sieve_project_schema(), 3L)
  expect_named(SIEVER:::sieve_project_steps(), c("1", "2"))

  # A schema 1 file (a plain RDS without the rows held aside and without
  # the content key) opens through that step, and the session says where
  # it came from.
  st <- ei_project_state()
  current <- unclass(SIEVER:::sieve_project_snapshot(st))
  old <- current
  old$schema_version <- 1L
  old$held_rows <- NULL
  old$keys$content <- NULL
  file <- tempfile(fileext = ".rds")
  saveRDS(old, file)
  read <- SIEVER:::sieve_project_read(file)
  expect_identical(read$schema_version, 3L)
  expect_identical(read$migrated_from, c(1L, 2L))
  expect_identical(read$held_rows, list())
  expect_identical(read$keys$content, SIEVER:::sieve_content_key(read))
  expect_identical(attr(read, "container"), 0L)
  expect_identical(SIEVER:::sieve_project_check(read), character(0))
  expect_match(SIEVER:::sieve_project_describe(read)[["schema"]], "3 \\(upgraded from 1\\)")
  expect_match(SIEVER:::sieve_project_describe(read)[["file_form"]], "plain RDS")
  opened <- sieve_new_state()
  SIEVER:::sieve_project_restore(opened, read, source = "old.rds")
  expect_identical(shiny::isolate(opened$items), current$items)
  expect_identical(
    shiny::isolate(opened$project[c("schema_version", "migrated_from", "container_version")]),
    list(schema_version = 3L, migrated_from = c(1L, 2L), container_version = 0L)
  )
})

test_that("a field the state gains later is kept by the snapshot, and a file cannot add or replace a field", {
  # A field no version of the state has yet stands for one added later.
  st <- ei_project_state()
  shiny::isolate(st$reviewer_notes <- list(N1 = list(item = "t2_cubes", note = "content coverage")))
  snap <- SIEVER:::sieve_project_snapshot(st)
  expect_identical(snap$other$reviewer_notes$N1$note, "content coverage")

  # A session whose state has that field gets it back; one without it does not.
  with_field <- sieve_new_state()
  shiny::isolate(with_field$reviewer_notes <- list())
  SIEVER:::sieve_project_restore(with_field, snap)
  expect_identical(shiny::isolate(with_field$reviewer_notes), shiny::isolate(st$reviewer_notes))
  without <- sieve_new_state()
  SIEVER:::sieve_project_restore(without, snap)
  expect_false("reviewer_notes" %in% names(shiny::isolate(shiny::reactiveValuesToList(without))))
  # A field of the opening session that the file does not hold is set to what
  # a new session starts with, not kept (so no decision of the opening
  # session is still there after another project is opened).
  lacking <- snap
  lacking$other$decisions <- NULL
  lacking$other$reviewer_notes <- NULL
  lacking$keys$content <- SIEVER:::sieve_content_key(lacking)
  stale <- sieve_new_state()
  shiny::isolate({
    stale$decisions <- list(note = "decided in the other project")
    stale$reviewer_notes <- list(N9 = "from the other project")
  })
  SIEVER:::sieve_project_restore(stale, lacking)
  expect_identical(shiny::isolate(stale$decisions), list())
  expect_null(shiny::isolate(stale$reviewer_notes))

  # The researcher's recorded decisions are such a field of the state: they
  # are saved with the project and come back when it is opened.
  decided <- ei_project_state()
  shiny::isolate(SIEVER:::sieve_record_decision(
    decided,
    topic = "factor retention", question = "How many factors are retained?",
    choice = "3", rationale = "Three a priori ability domains; parallel analysis agrees."
  ))
  kept <- SIEVER:::sieve_project_snapshot(decided)
  expect_length(kept$other$decisions, 1L)
  reopened <- sieve_new_state()
  SIEVER:::sieve_project_restore(reopened, kept)
  expect_identical(shiny::isolate(reopened$decisions[[1]]$choice), "3")
  expect_identical(
    shiny::isolate(reopened$decisions[[1]]$rationale),
    shiny::isolate(decided$decisions[[1]]$rationale)
  )

  # A file that tries to replace the checked data through the extra fields is refused.
  forged <- snap
  forged$other$raw <- datasets::mtcars
  expect_match(SIEVER:::sieve_project_check(forged), "repeat a field the project already holds")
  untouched <- sieve_new_state()
  expect_error(SIEVER:::sieve_project_restore(untouched, forged), class = "sieve_project_error")
  expect_null(shiny::isolate(untouched$raw))
})

# ---- The keying a file states, and the evidence an import weighs ------------

ei_keep <- function(from, ext) {
  to <- tempfile(fileext = ext)
  file.copy(from, to)
  to
}

# Session 1: a survey uploaded, constructs assigned, the
# specification downloaded before any reversal, then two items reverse scored
# in the grid and the analytic data and the specification downloaded again.
# The analytic values are also written as a CSV, which carries no labels.
ei_mismatched_pair <- function() {
  skip_if_not_installed("haven")
  k <- seq_len(60L)
  likert <- function(s, by) as.integer(((k * by + s) %% 5L) + 1L)
  survey <- data.frame(
    id = 1000L + k, q1 = likert(0L, 1L), q2 = likert(1L, 2L), q3 = likert(2L, 3L),
    q4 = likert(3L, 4L), q5 = likert(4L, 6L)
  )
  path <- ei_csv(survey)
  st <- sieve_new_state()
  f <- new.env(parent = emptyenv())
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, path)
    for (it in paste0("q", 1:5)) {
      session$setInputs(cell_edit = list(item = it, field = "construct", value = "Engagement"))
    }
  })
  f$spec_early <- ei_export(st, "processed")$spec
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "q4", field = "reverse", value = "Yes"))
    f$items <- state$items
    f$raw <- state$raw
    f$meta <- state$meta
  })
  late <- ei_export(st, "processed")
  f$sav <- late$data
  f$spec_late <- late$spec
  f$csv <- file.path(dirname(path), "data_processed.csv")
  utils::write.csv(cbind(id = f$raw$id, f$items), f$csv, row.names = FALSE)
  f$data <- survey
  f
}

test_that("a variable label states the keying in the one form SIEVER writes, and is read back without it", {
  suffix <- SIEVER:::sieve_reversed_suffix
  read <- SIEVER:::sieve_label_keying
  expect_identical(suffix(1, 5), " [reverse scored within 1 to 5]")
  expect_identical(suffix(1, 5, "observed"), " [reverse scored within the observed range 1 to 5]")
  expect_identical(suffix(NA_real_, NA_real_), " [reverse scored]")
  cases <- list(
    list(lo = 1, hi = 5, bounds = "declared"),
    list(lo = 0.1 + 0.2, hi = 4 + 1 / 3, bounds = "declared"),
    list(lo = 0.5, hi = 40.25, bounds = "observed")
  )
  for (case in cases) {
    label <- paste0("Looks forward, to work", suffix(case$lo, case$hi, case$bounds))
    got <- read(label)
    expect_identical(got$label, "Looks forward, to work")
    expect_true(got$reversed)
    expect_identical(c(got$lo, got$hi), c(case$lo, case$hi)) # the digits identify the double
    expect_identical(got$bounds, case$bounds)
  }
  plain <- read("q2 [reverse scored]")
  expect_true(plain$reversed)
  expect_true(is.na(plain$lo))
  expect_identical(plain$label, "q2")
  # Anything else says nothing: a label that mentions reverse scoring in its
  # own words, the statement not at the end, bounds that are not bounds, no
  # label at all.
  for (label in c(
    "q2 (reverse scored)", "q2 [reverse scored within 1 to 5] extra",
    "q2 [reverse scored within 5 to 1]", "q2 [reverse scored within a to b]", "q2",
    NA_character_
  )) {
    got <- read(label)
    expect_false(got$reversed, info = label)
    expect_identical(got$label, label, info = label)
  }
  expect_false(read(NULL)$reversed)
  # The statement is written once, whatever an earlier export left in the Item Name.
  d <- data.frame(a = c(1, 2, 5, 3))
  meta <- sieve_build_meta(d, "a")
  meta$label <- "a [reverse scored within 1 to 5]"
  r <- SIEVER:::sieve_reverse_item(d, meta, "a", lo = 1, hi = 5, loaded = d$a)
  expect_identical(
    attr(SIEVER:::sieve_export_data(d, r$data, r$meta, values = "processed")$a, "label"),
    "a [reverse scored within 1 to 5]"
  )
  expect_identical(attr(SIEVER:::sieve_export_data(d, r$data, r$meta, values = "raw")$a, "label"), "a")
})

test_that("the content keys of a specification tell the keying of the values now loaded, or say that they cannot", {
  key <- SIEVER:::sieve_values_key
  ev <- SIEVER:::sieve_keying_evidence
  adm <- c(1, 2, 5, NA, 3)
  rev <- (1 + 5) - adm
  # Saved with an analytic file that held the item reverse scored: the
  # analytic key is of the reversed values (reverse TRUE), the loaded key of
  # the administered ones (loaded_reversed FALSE).
  late <- function(column, loaded = NULL, bounds = list()) {
    ev(column, loaded, TRUE, FALSE, key(rev), key(adm), bounds)
  }
  expect_identical(late(rev)[c("holds", "by")], list(holds = TRUE, by = "values"))
  expect_identical(late(adm)[c("holds", "by")], list(holds = FALSE, by = "values"))
  expect_identical(late(as.integer(c(1L, 2L, 5L, NA, 3L)))$holds, FALSE) # storage does not matter
  # The session moved the item within other bounds: the values match no key,
  # the values as loaded do, so the current values are their reflection.
  moved <- late(8 - adm, loaded = adm)
  expect_identical(moved[c("holds", "by")], list(holds = TRUE, by = "loaded"))
  # Saved before any reversal: both keys are of the administered values and
  # say they are not reverse scored. Reverse scored values match neither,
  # and their reflection within the bounds tried does: the pair that matched
  # is reported, in the order the pairs were given.
  early <- function(column, loaded = NULL, bounds = list()) {
    ev(column, loaded, FALSE, FALSE, key(adm), key(adm), bounds)
  }
  r <- early(rev, loaded = rev, bounds = list(
    specification = c(NA_real_, NA_real_),
    stored = c(NA_real_, NA_real_), observed = c(1, 5)
  ))
  expect_identical(
    r[c("holds", "by", "lo", "hi", "bounds")],
    list(holds = TRUE, by = "reflection", lo = 1, hi = 5, bounds = "observed")
  )
  # A pair whose reflection does not give the saved values back is no match.
  r2 <- early(rev, loaded = rev, bounds = list(specification = c(0, 7), stored = c(1, 5)))
  expect_identical(r2$bounds, "stored")
  expect_identical(c(r2$lo, r2$hi), c(1, 5))
  # A pair outside which a value lies is not tried; other values cannot be told.
  expect_true(is.na(early(rev, loaded = rev, bounds = list(observed = c(2, 5)))$holds))
  expect_true(is.na(early(c(2, 2, 4, 1, 3), loaded = c(2, 2, 4, 1, 3), bounds = list(observed = c(1, 4)))$holds))
  expect_true(is.na(early(c(2, 2, 4, 1, 3))$holds))
  # Keys that point both ways: a damaged specification, said to be.
  bad <- ev(adm, NULL, TRUE, FALSE, key(adm), key(adm), list())
  expect_true(bad$conflict)
  expect_true(is.na(bad$holds))
  # No keys, or a column that is not numeric: nothing is shown.
  expect_true(is.na(ev(adm, NULL, TRUE, FALSE, "", "", list())$holds))
  expect_true(is.na(ev(c("a", "b"), NULL, TRUE, FALSE, key(adm), key(adm))$holds))
})

test_that("a specification saved before the reversals, restored on the processed data saved after them, returns the items to the direction it records", {
  f <- ei_mismatched_pair()
  expect_identical(f$meta$reverse, f$meta$item %in% c("q2", "q4"))
  expect_identical(f$items$q2, (1 + 5) - as.numeric(f$data$q2))
  for (kind in c("sav", "csv")) {
    st <- sieve_new_state()
    shiny::testServer(mod_ingest_server, args = list(state = st), {
      ei_load(session, f[[kind]])
      out <- SIEVER:::sieve_apply_spec(state, f$spec_early, keying = "verify", name = "early.csv")
      expect_true(out$ok, info = paste(kind, paste(out$problems, collapse = " | ")))
      expect_setequal(out$summary$returned, c("q2", "q4"))
      expect_match(out$message, "Returned to the administered direction: q2, q4", info = kind)
      for (it in c("q2", "q4")) {
        expect_identical(as.numeric(state$items[[it]]), as.numeric(f$data[[it]]), info = paste(kind, it))
        expect_false(state$meta$reverse[state$meta$item == it], info = paste(kind, it))
        expect_identical(state$meta$label[state$meta$item == it], it, info = paste(kind, it))
      }
      expect_identical(as.numeric(state$items$q1), as.numeric(f$data$q1), info = kind)
      expect_equal(stats::cor(state$items$q1, state$items$q2), stats::cor(f$data$q1, f$data$q2),
        tolerance = 1e-12, info = kind
      )
      expect_true(all(c("q2", "q4") %in% vapply(state$transforms, function(r) r$item, character(1))),
        info = kind
      )
      expect_identical(state$meta$construct, rep("Engagement", 5))
    })
  }
  # Other data (the first forty rows of the administered file) with the early
  # specification: the keys match nothing, so the import asks rather than
  # taking the reverse cell's FALSE as proof; under "administered" it applies
  # the specification and reverses nothing.
  other <- ei_csv(f$data[1:40, ], "other.csv")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, other)
    whole <- ei_whole(state)
    out <- SIEVER:::sieve_apply_spec(state, f$spec_early, keying = "verify")
    expect_false(out$ok)
    expect_match(
      paste(out$problems, collapse = "\n"),
      "lists q1, q2, q3, q4, q5 as not reverse keyed, but the values now loaded for them"
    )
    expect_identical(ei_whole(state), whole)
    out <- SIEVER:::sieve_apply_spec(state, f$spec_early, keying = "administered")
    expect_true(out$ok)
    expect_identical(state$meta$reverse, rep(FALSE, 5))
    expect_identical(lapply(state$items, as.numeric), lapply(state$raw[names(state$items)], as.numeric))
    expect_length(state$transforms, 0L)
  })
})

test_that("a processed file loaded alone is read as its labels say, the statement is written once, and a record the keys contradict stops the import", {
  f <- ei_mismatched_pair()
  # The .sav file: its labels say q2 and q4 hold reverse scored values within 1 to 5.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, f$sav)
    i <- match(c("q2", "q4"), state$meta$item)
    expect_identical(state$meta$reverse[i], c(TRUE, TRUE))
    expect_identical(state$meta$label[i], c("q2", "q4"))
    expect_identical(state$meta$rev_lo[i], c(1, 1))
    expect_identical(state$meta$rev_hi[i], c(5, 5))
    # The grid reversed them within the observed range (nothing was declared),
    # and the label says so.
    expect_identical(state$meta$rev_bounds[i], c("observed", "observed"))
    expect_identical(state$meta$reverse[-i], rep(FALSE, 3))
    expect_identical(lapply(state$items, as.numeric), lapply(state$raw[names(state$items)], as.numeric))
    recs <- state$transforms
    expect_identical(
      unname(vapply(recs, function(r) paste(r$type, r$item, r$origin), character(1))),
      c("keying q2 load", "keying q4 load")
    )
    expect_match(paste(state$codelog, collapse = "\n"), "already reverse scored in the data as read")
  })
  # A processed export labels the column once.
  expect_identical(
    attr(haven::read_sav(ei_export(st, "processed")$data)$q2, "label"),
    "q2 [reverse scored within the observed range 1 to 5]"
  )
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    # Its own specification sets the constructs and changes no value, no
    # keying and no record.
    whole <- ei_whole(state)
    out <- SIEVER:::sieve_apply_spec(state, f$spec_late, keying = "verify")
    expect_true(out$ok, info = paste(out$problems, collapse = " | "))
    expect_identical(state$items, whole$items)
    keyed <- c("item", "reverse", "rev_lo", "rev_hi", "rev_bounds", "status", "type", "label")
    expect_identical(state$meta[keyed], whole$meta[keyed])
    expect_identical(state$meta$construct, rep("Engagement", 5))
    expect_identical(state$transforms, whole$transforms)
    # Taking the reversal back in the grid returns the administered values exactly.
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "No"))
    expect_identical(as.numeric(state$items$q2), as.numeric(f$data$q2))
    expect_false(state$meta$reverse[state$meta$item == "q2"])
  })
  # The same values as a CSV carry no label, so they are read as administered.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, f$csv)
    expect_identical(state$meta$reverse, rep(FALSE, 5))
    # Reverse? = Yes then reflects them: the values are the administered ones
    # under a ledger that says reverse scored. Restoring the specification
    # saved with the file is refused, because its keys show the record to be
    # wrong, and the session is left as it is.
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "Yes"))
    expect_identical(as.numeric(state$items$q2), as.numeric(f$data$q2))
    expect_true(state$meta$reverse[state$meta$item == "q2"])
    whole <- ei_whole(state)
    out <- SIEVER:::sieve_apply_spec(state, f$spec_late, keying = "verify")
    expect_false(out$ok)
    said <- paste(out$problems, collapse = "\n")
    expect_match(said, "ledger of this session says q2 is reverse scored, but the specification's content keys show")
    expect_match(said, "records as administered")
    expect_match(said, "Load the data file again")
    expect_identical(ei_whole(state), whole)
  })
  # The export after the toggle labels the column once (as the ledger says).
  expect_identical(
    attr(haven::read_sav(ei_export(st, "processed")$data)$q2, "label"),
    "q2 [reverse scored within the observed range 1 to 5]"
  )
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    # Loaded again, the file with its own specification is recorded as it
    # should be: the keys match the reverse scored values.
    ei_load(session, f$csv, press = 2L)
    out <- SIEVER:::sieve_apply_spec(state, f$spec_late, keying = "verify")
    expect_true(out$ok, info = paste(out$problems, collapse = " | "))
    expect_setequal(out$summary$recorded, c("q2", "q4"))
    expect_identical(state$meta$reverse, state$meta$item %in% c("q2", "q4"))
    expect_identical(as.numeric(state$items$q2), f$items$q2)
  })
  # A label whose statement the values contradict is not recorded: the item
  # is loaded as administered, without the statement in its name.
  d <- f$data
  attr(d$q3, "label") <- "q3 wording [reverse scored within 1 to 3]" # q3 takes values up to 5
  bad <- tempfile(fileext = ".sav")
  haven::write_sav(d, bad)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    ei_load(session, bad)
    i <- match("q3", state$meta$item)
    expect_false(state$meta$reverse[i])
    expect_identical(state$meta$label[i], "q3 wording")
    expect_true(is.na(state$meta$rev_lo[i]))
    expect_length(state$transforms, 0L)
  })
})

# ---- The project file: a digest verified before anything is unserialized ---

test_that("a project file is a container whose content is verified before it is unserialized, and a damaged one is refused", {
  st <- ei_project_state()
  snap <- SIEVER:::sieve_project_snapshot(st)
  bytes <- SIEVER:::sieve_project_encode(snap)
  # The header, in the form the reader checks: the text up to the first
  # blank line.
  newlines <- which(bytes == as.raw(10L))
  head_end <- newlines[which(diff(newlines) == 1L)[1]]
  header <- rawToChar(bytes[seq_len(head_end - 1L)])
  lines <- strsplit(header, "\n", fixed = TRUE)[[1]]
  expect_length(lines, 4L)
  expect_identical(lines[1], "SIEVER project container 1")
  expect_identical(lines[2], "schema 3")
  expect_match(lines[3], "^payload gzip [0-9]+$")
  expect_match(lines[4], "^digest md5 [0-9a-f]{32}$")
  payload <- bytes[-seq_len(nchar(paste(c(lines, "", ""), collapse = "\n")))]
  expect_identical(length(payload), as.integer(sub("^payload gzip ", "", lines[3])))
  expect_identical(SIEVER:::sieve_bytes_digest(payload), sub("^digest md5 ", "", lines[4]))
  # Decoded, it is the snapshot.
  back <- SIEVER:::sieve_project_decode(bytes)
  expect_identical(back$container, 1L)
  expect_identical(back$schema, 3L)
  expect_identical(back$snapshot, snap)
  expect_identical(SIEVER:::sieve_project_check(back$snapshot), character(0))
  # Bytes that are not a container are not read as one.
  expect_null(SIEVER:::sieve_project_decode(serialize(snap, NULL)))
  expect_null(SIEVER:::sieve_project_decode(charToRaw("SIEVER project")))
  expect_null(SIEVER:::sieve_project_decode(raw(0)))
  # Every kind of damage is refused by the header or the digest, with a
  # reason, before the content is unserialized.
  refuse <- function(bytes, pattern) {
    expect_error(SIEVER:::sieve_project_decode(bytes), pattern, class = "sieve_project_error")
  }
  n <- length(bytes)
  refuse(bytes[-n], "holds [0-9]+ bytes of content where its header says")
  refuse(c(bytes, as.raw(0L)), "holds [0-9]+ bytes of content where its header says")
  one <- bytes
  one[n - 10L] <- xor(one[n - 10L], as.raw(1L))
  refuse(one, "content does not match the digest")
  head_len <- nchar(paste(c(lines, "", ""), collapse = "\n"))
  swap <- function(from, to) c(
    charToRaw(sub(from, to, rawToChar(bytes[seq_len(head_len)]), fixed = TRUE)),
    bytes[-seq_len(head_len)]
  )
  refuse(swap("digest md5 ", "digest md5 0"), "not in the form SIEVER writes")
  refuse(swap("payload gzip", "payload zip"), "not in the form SIEVER writes")
  refuse(swap("schema 3", "schema 3 "), "not in the form SIEVER writes")
  refuse(swap("container 1", "container 2"), "newer SIEVER \\(project file version 2")
  refuse(bytes[seq_len(head_len - 2L)], "header does not end")
  digest <- sub("^digest md5 ", "", lines[4])
  wrong <- paste0(substr(digest, 1, 31), if (substr(digest, 32, 32) == "0") "1" else "0")
  refuse(swap(digest, wrong), "content does not match the digest")
  # The file as written and read.
  file <- tempfile(fileext = ".rds")
  SIEVER:::sieve_project_write(snap, file)
  expect_identical(readBin(file, "raw", file.size(file))[seq_len(head_len)], bytes[seq_len(head_len)])
  read <- SIEVER:::sieve_project_read(file)
  expect_identical(attr(read, "container"), 1L)
  expect_identical(unclass(read)[names(snap)], unclass(snap)[names(snap)])
})

test_that("one flipped bit anywhere in a project file leaves it refused or identical, never different, and never crashes", {
  # A sweep of flipped bits (bit mask 1), bounded to a few thousand
  # positions: no position may open with different content, crash R, or
  # hang it.
  d <- ei_survey()[, c("id", "sex", "q1", "q2", "q3", "q4", "q5")]
  st <- ei_state(d)
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "q5", field = "status", value = "Dropped"))
    session$setInputs(cell_edit = list(item = "q1", field = "construct", value = "Engagement"))
    session$setInputs(confirm = 1L)
  })
  file <- tempfile(fileext = ".rds")
  SIEVER:::sieve_project_write(SIEVER:::sieve_project_snapshot(st), file)
  bytes <- readBin(file, "raw", file.size(file))
  reference <- unclass(SIEVER:::sieve_project_read(file))
  attr(reference, "container") <- NULL
  positions <- if (length(bytes) <= 4000L) seq_along(bytes) else unique(round(seq(1,
    length(bytes),
    length.out = 4000L
  )))
  outcome <- character(length(positions))
  tmp <- tempfile(fileext = ".rds")
  for (k in seq_along(positions)) {
    b <- bytes
    b[positions[k]] <- xor(b[positions[k]], as.raw(1L))
    writeBin(b, tmp)
    got <- tryCatch(
      {
        x <- unclass(SIEVER:::sieve_project_read(tmp))
        attr(x, "container") <- NULL
        if (identical(x, reference)) "identical" else "different"
      },
      sieve_project_error = function(e) "refused",
      error = function(e) paste("error", class(e)[1])
    )
    outcome[k] <- got
  }
  counts <- table(outcome)
  expect_true(all(names(counts) %in% c("refused", "identical")), info = paste(names(counts), collapse = ", "))
  expect_identical(sum(outcome == "different"), 0L)
  expect_gt(sum(outcome == "refused"), length(positions) * 0.9)
  # A flip in a header byte that the grammar does not see (none: every
  # header byte is checked), and one in the payload, are refused by the
  # digest; the identical ones are flips the reader never reads (the bytes
  # after the header that the header's length excludes: none here).
  expect_identical(length(outcome), length(positions))
})

test_that("a project whose parts do not agree is refused, with the reason, whether or not its keys were recomputed", {
  # Each case is a snapshot taken through the real observers and
  # then changed, with every key recomputed so that only the consistency
  # checks can catch it.
  d <- ei_survey()[, c("id", "q1", "q2", "q3", "q4", "q5")]
  st <- ei_state(d)
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "Yes"))
    session$setInputs(confirm = 1L)
    shiny::isolate(state$analyses <- list(A1 = list(
      id = "A1",
      kind = "cfa",
      revision = SIEVER:::sieve_revision(state)
    )))
  })
  good <- unclass(SIEVER:::sieve_project_snapshot(st))
  expect_identical(SIEVER:::sieve_project_check(good), character(0))
  rekey <- function(s) {
    s$keys$raw <- SIEVER:::sieve_data_key(s$raw)
    s$keys$items <- SIEVER:::sieve_data_key(s$items)
    s$keys$meta <- SIEVER:::sieve_meta_key(s$meta)
    s$keys$ledger <- SIEVER:::sieve_ledger_key(s$meta)
    s$keys$roles <- SIEVER:::sieve_roles_key(s$roles)
    s$revision$data_key <- s$keys$items
    s$revision$meta_key <- s$keys$meta
    s$keys$content <- SIEVER:::sieve_content_key(s)
    s
  }
  odd <- list(
    "values of q3 are neither the values as loaded nor their reflection" = {
      x <- good
      x$items$q3 <- rev(x$items$q3)
      rekey(x)
    },
    "q4 is reverse scored by the ledger or by the values, and no transformation record" = {
      x <- good
      x$meta$reverse[x$meta$item == "q4"] <- TRUE
      x$meta$rev_lo[x$meta$item == "q4"] <- 1
      x$meta$rev_hi[x$meta$item == "q4"] <- 5
      rekey(x)
    },
    "values of q4 are neither the values as loaded nor their reflection" = {
      x <- good
      x$items$q4 <- (1 + 5) - as.numeric(x$items$q4) # reflected, with no bounds and no record
      rekey(x)
    },
    "measurement type that is not one of continuous, ordered, binary, constant, text \\(likert\\)" = {
      x <- good
      x$meta$type[1] <- "likert"
      rekey(x)
    },
    "item types are confirmed but no estimator is recorded" = {
      x <- good
      x$estimator <- NULL
      x$confirmed <- TRUE
      rekey(x)
    },
    "thresholds are not a named list of numbers" = {
      x <- good
      x$thresholds <- list(load_low = "high")
      rekey(x)
    },
    "transformation record names an item that is not in the data, an operation that does not exist, or a dataset the session never loaded \\(record 2\\)" = {
      x <- good
      x$transforms <- c(x$transforms, list(T9 = list(type = "reverse", item = "nope", dataset_id = "D77")))
      rekey(x)
    },
    "dataset the session never loaded \\(record 2\\)" = {
      x <- good
      x$transforms <- c(x$transforms, list(T9 = list(type = "reverse", item = "q1", dataset_id = "D77")))
      rekey(x)
    },
    "ledger rows held aside are not a list named by variable" = {
      x <- good
      x$held_rows <- list(x$meta[1, ])
      rekey(x)
    },
    "ledger row held aside does not describe a non item column of the data \\(q1\\)" = {
      x <- good
      x$held_rows <- list(q1 = x$meta[1, ])
      rekey(x)
    },
    "analysis registry holds something that is not a record" = {
      x <- good
      x$analyses <- list(A1 = "a string")
      rekey(x)
    }
  )
  fresh <- sieve_new_state()
  for (nm in names(odd)) {
    problems <- SIEVER:::sieve_project_check(odd[[nm]])
    expect_gt(length(problems), 0L, label = nm)
    expect_match(paste(problems, collapse = "; "), nm, info = nm)
    expect_error(SIEVER:::sieve_project_restore(fresh, structure(odd[[nm]], class = "sieve_project")),
      class = "sieve_project_error", info = nm
    )
    expect_null(shiny::isolate(fresh$raw), info = nm)
  }
  # A consistent change passes the same checks: the keys recomputed after a
  # change the service could have made (q2 taken back, with its record).
  ok <- good
  ok$items$q2 <- ok$raw$q2
  ok$meta$reverse[ok$meta$item == "q2"] <- FALSE
  ok$meta$mean[ok$meta$item == "q2"] <- mean(ok$raw$q2, na.rm = TRUE)
  ok$transforms$T2 <- ok$transforms$T1
  ok$transforms$T2$id <- "T2"
  ok$transforms$T2$from <- TRUE
  ok$transforms$T2$to <- FALSE
  expect_identical(SIEVER:::sieve_project_check(rekey(ok)), character(0))
})

test_that("what a project file holds is said before it is opened", {
  st <- ei_project_state()
  file <- tempfile(fileext = ".rds")
  SIEVER:::sieve_project_write(SIEVER:::sieve_project_snapshot(st), file)
  about <- SIEVER:::sieve_project_describe(SIEVER:::sieve_project_read(file))
  expect_identical(about[["dataset"]], sieve_hs9_fixture()$name)
  # The rows say whether respondents were removed.
  expect_identical(about[["rows"]], "301 (none removed)")
  expect_identical(about[["items"]], "9 (8 included, 1 excluded)")
  expect_identical(about[["roles"]], "9 items, 1 identifier, 3 groups, 1 criterion")
  expect_match(about[["constructs"]], "^3 \\(Visual, Textual, Speed\\)$")
  expect_identical(about[["reverse_scored"]], "2")
  # The estimator label states the resolved estimator, not "ML or MLR":
  # ML with FIML, DMAR's default, for these continuous items.
  expect_match(about[["confirmed"]], "^yes \\(ML with FIML\\)$")
  expect_identical(about[["records"]], "2")
  expect_identical(about[["analyses"]], "1")
  expect_match(about[["steps"]], "^[0-9]+$")
  expect_match(about[["saved"]], "^[0-9]{4}-[0-9]{2}-[0-9]{2} .* \\(R [0-9.]+\\)$")
  expect_identical(about[["siever"]], SIEVER:::sieve_version())
  expect_identical(about[["schema"]], "3")
  expect_identical(about[["file_form"]], "project file version 1, digest verified")
})

# ---- A ledger row is held aside when its column leaves the pool -------------

test_that("a column that leaves the item pool keeps its ledger row aside and gets it back on return", {
  d <- ei_survey()
  roles <- SIEVER:::sieve_propose_roles(d)
  pool <- roles$variable[roles$role == "item"]
  items <- d[, pool, drop = FALSE]
  meta <- sieve_build_meta(d, pool, labels = stats::setNames(pool, pool))
  # Everything a researcher can set on a row, set by hand.
  at <- match("q3", meta$item)
  meta$label[at] <- "Third item (renamed)"
  meta$wording[at] <- "The wording of the third item"
  meta$construct[at] <- "Strain"
  meta$facet[at] <- "pace"
  meta$type[at] <- "ordered"
  meta$status[at] <- "parked"
  meta$rev_lo[at] <- 1
  meta$rev_hi[at] <- 5
  meta$rev_bounds[at] <- "declared"
  meta$note[at] <- "a note"
  row_before <- SIEVER:::sieve_meta_keying(meta)[at, ]

  out <- SIEVER:::sieve_set_roles(d, items, meta, roles, c(q3 = "auxiliary"))
  expect_false("q3" %in% names(out$items))
  expect_false("q3" %in% out$meta$item)
  expect_named(out$held, "q3")
  expect_identical(out$held$q3[, names(row_before)], row_before)
  # Back: the row returns exactly, excluded status included, and nothing is held.
  back <- SIEVER:::sieve_set_roles(d, out$items, out$meta, out$roles, c(q3 = "item"), held = out$held)
  expect_identical(back$meta, SIEVER:::sieve_meta_keying(meta))
  expect_identical(back$items, items)
  expect_identical(back$held, list())
  expect_identical(back$meta$status[back$meta$item == "q3"], "parked")
  # A column that never was an item gets a fresh row; a held row of another
  # shape (another ledger layout) is not forced on the pool.
  fresh <- SIEVER:::sieve_set_roles(d, items, meta, roles, c(sex = "item"), held = out$held)
  expect_identical(fresh$meta$label[fresh$meta$item == "sex"], "sex")
  expect_named(fresh$held, "q3")
  odd <- out$held
  odd$q3$extra_column <- 1
  forced <- SIEVER:::sieve_set_roles(d, out$items, out$meta, out$roles, c(q3 = "item"), held = odd)
  expect_identical(forced$meta$label[forced$meta$item == "q3"], "q3")
  expect_named(forced$held, "q3")
  # A reverse scored item still cannot leave the pool: its values would come
  # back as loaded while its row says they are reflected.
  r <- SIEVER:::sieve_reverse_item(items, meta, "q4", lo = 1, hi = 5, loaded = d$q4)
  expect_error(SIEVER:::sieve_set_roles(d, r$data, r$meta, roles, c(q4 = "auxiliary")),
    "reverse scored here",
    class = "sieve_role_error"
  )
})

test_that("a role change out of the item pool and back, in the grid, loses nothing of the item", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    ei_quiet(session$setInputs(load = 1L))
    it <- state$meta$item[4]
    bounds <- tempfile(fileext = ".csv")
    utils::write.csv(data.frame(variable = it, rev_lo = "0", rev_hi = "20"), bounds, row.names = FALSE)
    session$setInputs(up_spec = list(name = "bounds.csv", datapath = bounds))
    session$setInputs(cell_edit = list(item = it, field = "label", value = "Paragraph comprehension (renamed)"))
    session$setInputs(cell_edit = list(item = it, field = "type", value = "ordered"))
    session$setInputs(cell_edit = list(item = it, field = "status", value = "Dropped"))
    session$setInputs(confirm = 1L)
    before <- ei_whole(state)
    row_before <- state$meta[state$meta$item == it, ]
    expect_identical(row_before$status, "parked")
    expect_identical(c(row_before$rev_lo, row_before$rev_hi, row_before$rev_bounds), c("0", "20", "declared"))

    session$setInputs(cell_edit = list(item = it, field = "role", value = "Auxiliary"))
    expect_false(it %in% state$meta$item)
    expect_false(it %in% names(state$items))
    expect_true(it %in% sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")))
    expect_named(state$held_rows, it)
    expect_match(paste(state$codelog, collapse = "\n"), "its ledger row is kept aside")
    session$setInputs(cell_edit = list(item = it, field = "role", value = "Item", n = 2L))
    after <- ei_whole(state)
    expect_identical(state$meta[state$meta$item == it, names(row_before)], row_before)
    expect_identical(after$meta, before$meta)
    expect_identical(after$items, before$items)
    expect_identical(after$roles, before$roles)
    expect_identical(after$id_vars, before$id_vars)
    expect_identical(state$held_rows, list())
    expect_true(SIEVER:::sieve_same_revision(SIEVER:::sieve_revision(state), SIEVER:::sieve_revision(state)))
    expect_identical(SIEVER:::sieve_meta_key(after$meta), SIEVER:::sieve_meta_key(before$meta))
  })
})

test_that("the rows held aside are saved with the project, return with it, and are dropped when other data are loaded", {
  st <- ei_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "q3", field = "label", value = "Third"))
    session$setInputs(cell_edit = list(item = "q3", field = "status", value = "Dropped"))
    session$setInputs(cell_edit = list(item = "q3", field = "role", value = "Auxiliary"))
    expect_named(state$held_rows, "q3")
    session$setInputs(confirm = 1L)
  })
  file <- tempfile(fileext = ".rds")
  snap <- SIEVER:::sieve_project_snapshot(st)
  expect_named(snap$held_rows, "q3")
  SIEVER:::sieve_project_write(snap, file)
  opened <- sieve_new_state()
  SIEVER:::sieve_project_restore(opened, SIEVER:::sieve_project_read(file))
  expect_identical(shiny::isolate(opened$held_rows), shiny::isolate(st$held_rows))
  shiny::testServer(mod_ingest_server, args = list(state = opened), {
    session$setInputs(cell_edit = list(item = "q3", field = "role", value = "Item"))
    expect_identical(state$meta$label[state$meta$item == "q3"], "Third")
    expect_identical(state$meta$status[state$meta$item == "q3"], "parked")
    expect_identical(state$held_rows, list())
    # Other data: nothing held aside carries over.
    session$setInputs(cell_edit = list(item = "q3", field = "role", value = "Auxiliary", n = 2L))
    expect_named(state$held_rows, "q3")
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    ei_quiet(session$setInputs(load = 1L))
    # The opened project is a study in the session, so Load asks first.
    ei_quiet(session$setInputs(load_replace = 1L))
    expect_identical(state$held_rows, list())
  })
})

# ---- The role proposal: numbered series and coded identifiers ---------------

test_that("a numbered series is proposed as items whatever its stem says, and a column of distinct whole number codes as an identifier", {
  kk <- 1:120
  d <- data.frame(
    pcode = 100000L + (kk * 7919L) %% 899981L, # six digit codes, every one different, not consecutive
    cond1 = (kk %% 5) + 1, cond2 = ((kk * 2) %% 5) + 1, cond3 = ((kk * 3) %% 5) + 1,
    time_1 = (kk %% 7) + 1, time_2 = ((kk * 3) %% 7) + 1,
    class_3 = ((kk * 7) %% 5) + 1, # a number at the end, but no sibling shares the stem
    weight1 = (kk %% 4) + 1, # likewise: read by its name
    female = kk %% 2,
    tx = kk %% 2, # no telling name: an item (two values)
    y = (kk * 0.37) %% 11
  )
  pr <- SIEVER:::sieve_propose_roles(d)
  expect_identical(
    stats::setNames(pr$role, pr$variable),
    c(
      pcode = "identifier", cond1 = "item", cond2 = "item", cond3 = "item", time_1 = "item",
      time_2 = "item", class_3 = "group", weight1 = "auxiliary", female = "group", tx = "item",
      y = "item"
    )
  )
  expect_match(pr$reason[pr$variable == "cond2"], "numbered series cond")
  expect_match(pr$reason[pr$variable == "pcode"], "whole numbers, every one different \\(a code")
  # The floor for a code: below it, distinct whole numbers that are not
  # consecutive are an item (consecutive ones number the rows at any size).
  few <- data.frame(a = c(3, 9, 1, 7, 5, 2, 8, 4, 12), b = 1)
  expect_identical(SIEVER:::sieve_propose_roles(few)$role[1], "item")
  expect_identical(SIEVER:::sieve_propose_roles(rbind(few, data.frame(a = 11, b = 1)))$role[1], "identifier")
  expect_identical(SIEVER:::sieve_propose_roles(rbind(few, data.frame(a = 11, b = 1)), min_coded = 11L)$role[1], "item")
  expect_identical(
    SIEVER:::sieve_propose_roles(data.frame(a = c(3, 9, 1, 7, 5, 2, 8, 4, 6), b = 1))$role[1],
    "identifier"
  )
  # A series needs a sibling that is numbered too: school and school_2 are not a series.
  pair <- data.frame(school = rep(1:3, 10), school_2 = rep(1:5, 6), q_1 = rep(1:5, 6), q_2 = rep(2:6, 6))
  roles <- SIEVER:::sieve_propose_roles(pair)
  expect_identical(
    stats::setNames(roles$role, roles$variable),
    c(school = "group", school_2 = "group", q_1 = "item", q_2 = "item")
  )
  # The survey file of these tests is proposed as in the tests above.
  s <- SIEVER:::sieve_propose_roles(ei_survey())
  expect_identical(
    s$role[s$variable %in% c("id", "sex", "gpa", "site")],
    c("identifier", "group", "criterion", "group")
  )
  expect_true(all(s$role[s$variable %in% c(paste0("q", 1:7), "perf")] == "item"))
})

# ---- Declared groups --------------------------------------------------------

# ---- In-cell edits stay in place --------------------------------------------

# The Ingest grid's cells are the Item Name, Use, Type, Reverse? and
# Construct (a facet is set on the Items tab). An edit that changes only its
# own cell flows into the ledger without a redraw, so the grid keeps its
# place; an edit that changes other cells or rows redraws once.
test_that("an in-cell edit of the type, construct, facet, name or keying updates the ledger without redrawing the grid; a status or a role change redraws it", {
  st <- ei_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    drawn <- rebuild()
    session$setInputs(cell_edit = list(item = "q1", field = "type", value = "continuous"))
    expect_identical(state$meta$type[state$meta$item == "q1"], "continuous")
    session$setInputs(cell_edit = list(item = "q1", field = "construct", value = "Engagement"))
    expect_identical(state$meta$construct[state$meta$item == "q1"], "Engagement")
    session$setInputs(cell_edit = list(item = "q1", field = "facet", value = "vigor"))
    expect_identical(state$meta$facet[state$meta$item == "q1"], "vigor")
    session$setInputs(cell_edit = list(item = "q1", field = "label", value = "First item"))
    expect_identical(state$meta$label[state$meta$item == "q1"], "First item")
    # None of these rebuilt the table: the edited cell keeps its place.
    expect_identical(rebuild(), drawn)
    # A reversal changes the summaries and the distribution of its row: the
    # row is updated in place, not the grid.
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "Yes"))
    expect_true(state$meta$reverse[state$meta$item == "q1"])
    expect_identical(rebuild(), drawn)
    # An exclusion moves the row below the Unused items line, and a role
    # change changes the rows of both grids: each redraws (once).
    session$setInputs(cell_edit = list(item = "q2", field = "status", value = "Dropped"))
    expect_identical(rebuild(), drawn + 1L)
    session$setInputs(cell_edit = list(item = "perf", field = "role", value = "Criterion"))
    expect_identical(rebuild(), drawn + 2L)
    # A refused edit redraws too, so the cell shows the value the item has.
    session$setInputs(cell_edit = list(item = "site", field = "role", value = "Item"))
    expect_identical(rebuild(), drawn + 3L)
    # A facet on an item without a construct is refused (a facet needs a
    # construct) and the grid redraws so the cell shows what the item has.
    session$setInputs(cell_edit = list(item = "q3", field = "facet", value = "vigor"))
    expect_true(is.na(state$meta$facet[state$meta$item == "q3"]))
    expect_identical(rebuild(), drawn + 4L)
  })
})

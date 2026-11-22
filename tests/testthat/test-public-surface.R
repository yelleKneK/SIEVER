# test-public-surface.R
#
# The public surface: the modification indices under a name that cannot be
# read as multiple imputation, the export triage (what stays exported because
# an article, the README, a help page or the recorded R relies on it, and
# which services are marked experimental), ggplot2 and bslib (>= 0.8.0) in
# Imports, the Description's references, one SVG map for the help page, the
# README and the articles, and no DMAR capability probe for what DMAR 1.1.0
# provides.
#
# Author: Ken Kelley

surface_exports <- function() getNamespaceExports("SIEVER")

surface_rd <- function() {
  man <- file.path("..", "..", "man")
  if (dir.exists(man)) tools::Rd_db(dir = file.path("..", "..")) else tools::Rd_db("SIEVER")
}

surface_rd_section <- function(rd, tag) {
  out <- character()
  for (el in rd) {
    if (identical(attr(el, "Rd_tag"), tag)) out <- c(out, paste(unlist(el), collapse = ""))
  }
  gsub("[[:space:]]+", " ", paste(out, collapse = " "))
}

surface_experimental <- c(
  "sieve_irt_bivariate_table", "sieve_irt_category_probabilities", "sieve_irt_empirical_traces",
  "sieve_irt_information_bands", "sieve_irt_parameter_evidence", "sieve_irt_residual_pairs",
  "sieve_longitudinal_spec", "sieve_run_longitudinal", "sieve_longitudinal_compare",
  "sieve_longitudinal_contrasts", "sieve_anchor_sensitivity", "sieve_ordinal_fit_impact"
)

# ---- the modification indices, by name --------------------------------------

test_that("the modification indices are sieve_modification_indices(), with no alias, and R-squared keeps its capital R", {
  ex <- surface_exports()
  expect_true("sieve_modification_indices" %in% ex)
  expect_false("sieve_mi_table" %in% ex)
  expect_false(exists("sieve_mi_table", envir = asNamespace("SIEVER"), inherits = FALSE))
  # "mi" names multiple imputation alone.
  expect_true(all(c("sieve_mi_plan", "sieve_mi_model") %in% ex))
  expect_true("sieve_R2_intervals" %in% ex)
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9",
    data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
  mi <- sieve_modification_indices(fit, top = 3L)
  expect_identical(nrow(mi$table), 3L)
  expect_identical(names(mi$table)[1:3], c("Path", "Type", "MI"))
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  hits <- unlist(lapply(c(
    list.files(file.path(root, "R"), full.names = TRUE),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE),
    file.path(root, c("README.md", "_pkgdown.yml", "NAMESPACE"))
  ), function(f) grep("sieve_mi_table", readLines(f, warn = FALSE), fixed = TRUE, value = TRUE)))
  expect_identical(hits, character(0))
})

# ---- the export triage -----------------------------------------------------------

test_that("the plumbing exports stay exported where an article or the recorded R relies on them", {
  ex <- surface_exports()
  # The fit store and its identity run in the scripted workflows article; the
  # session script the Reproducible Code page writes calls the replay
  # binders as SIEVER::; the item search's ordering is the article's
  # scripted route; the diagram update is in Getting Started and ?SIEVER.
  expect_true(all(c(
    "sieve_fit_store", "sieve_fit_identity", "sieve_replay_bind", "sieve_replay_engine_check",
    "sieve_order_candidates", "sieve_diagram_update"
  ) %in% ex))
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  # Every SIEVER:: call the recorded R writes names an exported function, so
  # a recorded script never needs `:::`.
  src <- unlist(lapply(list.files(file.path(root, "R"), full.names = TRUE), readLines, warn = FALSE))
  called <- unique(unlist(regmatches(src, gregexpr("SIEVER::[A-Za-z_.][A-Za-z0-9_.]*", src))))
  called <- sub("^SIEVER::", "", called)
  expect_true(length(called) >= 10L)
  expect_identical(setdiff(called, ex), character(0))
})

test_that("the advanced services are marked experimental in their help, and only they", {
  ex <- surface_exports()
  expect_true(all(surface_experimental %in% ex))
  rd <- surface_rd()
  alias_of <- lapply(rd, function(x) {
    a <- character()
    for (el in x) if (identical(attr(el, "Rd_tag"), "\\alias")) a <- c(a, paste(unlist(el), collapse = ""))
    a
  })
  marked <- character()
  for (i in seq_along(rd)) {
    desc <- surface_rd_section(rd[[i]], "\\description")
    if (grepl("Experimental: the arguments and the form of the result may change", desc, fixed = TRUE)) {
      marked <- c(marked, intersect(alias_of[[i]], ex))
    }
  }
  expect_setequal(marked, surface_experimental)
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  readme <- gsub("[[:space:]]+", " ", paste(readLines(file.path(root, "README.md"), warn = FALSE), collapse = " "))
  expect_match(readme, "Twelve of its functions are marked experimental in their help", fixed = TRUE)
})

test_that("every export is named in ?SIEVER or the scripted workflows article's table, as ?SIEVER says", {
  # ?SIEVER says the article's table names each export ?SIEVER does not,
  # beside the workflow it serves (not "area by area"), and every export is
  # named in one of the two.
  ex <- surface_exports()
  rd <- surface_rd()
  pkg <- rd[[grep("^SIEVER-package[.]Rd$", names(rd))]]
  pkg_text <- gsub("[[:space:]]+", " ", paste(unlist(pkg), collapse = " "))
  expect_match(pkg_text, "names each of the other exported functions beside the workflow it serves", fixed = TRUE)
  expect_false(grepl("area by area", pkg_text, fixed = TRUE))
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  art <- readLines(file.path(root, "vignettes", "scripted-workflows.Rmd"), warn = FALSE)
  table <- art[grepl("^[|]", art)]
  named <- function(f) {
    any(grepl(paste0("(^|[^A-Za-z0-9_.])", f, "([^A-Za-z0-9_.]|$)"), c(table, pkg_text)))
  }
  expect_identical(ex[!vapply(ex, named, logical(1))], character(0))
})

test_that("the scripted workflows article says which fits the session's fit store holds", {
  # Only the correlated-factors CFA fits of the CFA, Items, Validity and
  # Reliability pages and of the assistant go through the store, and the
  # article says so (not "every fit of the application's pages").
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  prose <- gsub("[[:space:]]+", " ", paste(readLines(file.path(root, "vignettes", "scripted-workflows.Rmd"),
    warn = FALSE), collapse = " "))
  expect_false(grepl("every fit of the application's pages", prose, fixed = TRUE))
  expect_match(prose, "the CFA, Items and Validity pages, the measurement model behind the Reliability page's factor scores, and the assistant reuse an identical fit", fixed = TRUE)
  expect_match(prose, "The store holds those fits alone: the exploratory, item response, structural, bifactor, invariance and Optimizer models are fit where they are requested", fixed = TRUE)
  # The code agrees: the session's store is read by those five files alone
  # (and created and emptied by the state helpers).
  files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
  uses <- vapply(files, function(f) {
    code <- readLines(f, warn = FALSE)
    any(grepl("state$fit_store", code[!grepl("^[[:space:]]*#", code)], fixed = TRUE))
  }, logical(1))
  expect_setequal(setdiff(basename(files[uses]), c("utils_contract.R", "utils_state.R")),
    c("mod_cfa.R", "mod_winnow.R", "mod_validity.R", "mod_reliab.R", "fct_assistant.R"))
})

# ---- ggplot2 and bslib in Imports ---------------------------------------------------

test_that("ggplot2 and bslib (>= 0.8.0) are imported, and the plots and the sidebar have no fallback", {
  desc <- utils::packageDescription("SIEVER")
  imports <- gsub("[[:space:]]+", " ", desc$Imports)
  suggests <- gsub("[[:space:]]+", " ", desc$Suggests)
  expect_match(imports, "ggplot2 (>= 3.4.0)", fixed = TRUE)
  expect_false(grepl("ggplot2", suggests, fixed = TRUE))
  expect_match(imports, "bslib (>= 0.8.0)", fixed = TRUE)
  expect_true("toggle_sidebar" %in% getNamespaceExports("bslib"))
  expect_false(exists("sieve_sidebar_toggle", envir = asNamespace("SIEVER"), inherits = FALSE))
  # ggplot2 is not an optional package: not in the notice's registry, and
  # the distribution drawing is a ggplot whatever sieve_have() answers.
  expect_false("ggplot2" %in% sieve_optional_packages()$package)
  d <- sieve_distribution_data(c(1, 2, 2, 3), "q1", role = "item", lo = 1, hi = 5)
  local_mocked_bindings(sieve_have = function(pkg) FALSE, .package = "SIEVER")
  expect_s3_class(sieve_distribution_plot(d), "ggplot")
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  src <- unlist(lapply(list.files(file.path(root, "R"), full.names = TRUE), readLines, warn = FALSE))
  expect_false(any(grepl("sieve_have(\"ggplot2\")", src, fixed = TRUE)))
  readme <- readLines(file.path(root, "README.md"), warn = FALSE)
  expect_false(any(grepl("`ggplot2` | some plots use base graphics", readme, fixed = TRUE)))
})

test_that("the scree, parallel analysis, structures and distribution figures are ggplot2's, and the heatmap and factor plot base graphics", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  draws <- function(f, pattern) any(grepl(pattern, deparse(get(f, envir = asNamespace("SIEVER"))), fixed = TRUE))
  # These figures are ggplot2's ...
  for (f in c("sieve_scree_plot", "sieve_pa_plot", "sieve_structures_plot", "sieve_distribution_plot")) {
    expect_true(draws(f, "ggplot2::ggplot("), info = f)
  }
  # ... and these two exported figures are base graphics, as the README and
  # ?SIEVER say of the path diagrams.
  for (f in c("sieve_correlation_heatmap", "sieve_efa_factor_plot")) {
    expect_true(draws(f, "graphics::"), info = f)
    expect_false(draws(f, "ggplot2::"), info = f)
  }
  readme <- gsub("[[:space:]]+", " ", paste(readLines(file.path(root, "README.md"), warn = FALSE), collapse = " "))
  expect_match(readme, "base R graphics for its model diagrams", fixed = TRUE)
})

# ---- run_app(): the number of workers ---------------------------------------------------

test_that("the help, the README and the user guide say what sieve.cores sets: a setting honored up to 64, a default of at most 8", {
  # What sieve_cores() does: an explicit setting is honored up to 64 for
  # the bootstraps, the Items diagnostics and the exploratory fits, and the
  # default is the cores less one, at most 8.
  withr::with_options(list(sieve.cores = 16L), {
    expect_identical(sieve_cores(), 16L)
    expect_identical(sieve_cores(max = 64L), 16L)
  })
  withr::with_options(list(sieve.cores = 4L), {
    expect_identical(sieve_cores(), 4L)
    expect_identical(sieve_cores(max = 64L), 4L)
  })
  # The parallel analysis takes no workers.
  expect_false("workers" %in% names(formals(sieve_parallel_analysis)))
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  flat <- function(path) gsub("[[:space:]]+", " ", paste(readLines(file.path(root, path), warn = FALSE), collapse = " "))
  rd <- flat(file.path("man", "run_app.Rd"))
  expect_match(rd, "the number of worker processes for the work that runs on several", fixed = TRUE)
  expect_match(rd, "(the parallel analysis runs in one process)", fixed = TRUE)
  expect_match(rd, "A setting is honored up to 64.", fixed = TRUE)
  expect_false(grepl("used as 8", rd, fixed = TRUE))
  for (path in c("README.md", file.path("vignettes", "user-guide.Rmd"))) {
    text <- flat(path)
    row <- regmatches(text, regexpr("[|] `sieve.cores` [|][^|]*[|][^|]*[|]", text))
    expect_length(row, 1L)
    expect_match(row, "(the parallel analysis runs in one process)", fixed = TRUE, info = path)
    expect_match(row, "A setting is honored up to 64.", fixed = TRUE, info = path)
    expect_match(row, "at most 2 under `R CMD check`", fixed = TRUE, info = path)
    expect_false(grepl("used as 8", row, fixed = TRUE), info = path)
  }
})

# ---- the README's screenshot ------------------------------------------------------------

test_that("the README's screenshot is the five-workspace application on the Items page, small enough to ship", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  readme <- readLines(file.path(root, "README.md"), warn = FALSE)
  line <- grep("readme-items-tab.png", readme, fixed = TRUE, value = TRUE)
  expect_identical(line, "![The Items page in the Structure workspace, with the bundled nine-test example](man/figures/readme-items-tab.png)")
  png <- file.path(root, "man", "figures", "readme-items-tab.png")
  expect_true(file.exists(png))
  expect_lt(file.size(png), 300 * 1024)
  # A PNG of 1440 by 900 pixels (the width and height in its IHDR chunk).
  con <- file(png, "rb")
  head <- readBin(con, "raw", 24L)
  close(con)
  expect_identical(head[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  dims <- c(sum(as.integer(head[17:20]) * 256^(3:0)), sum(as.integer(head[21:24]) * 256^(3:0)))
  expect_identical(dims, c(1440, 900))
})

# ---- the Description's references ---------------------------------------------------

test_that("the Description cites its methods with a year and a DOI each", {
  text <- gsub("[[:space:]]+", " ", utils::packageDescription("SIEVER")$Description)
  refs <- c(
    "Kelley and Pornprasertmanit (2016) <doi:10.1037/a0040086>",
    "Maxwell, Kelley, and Rausch (2008) <doi:10.1146/annurev.psych.59.103006.093735>",
    "Kelley and Lai (2011) <doi:10.1080/00273171.2011.543027>",
    "Rosseel (2012) <doi:10.18637/jss.v048.i02>"
  )
  for (r in refs) expect_match(text, r, fixed = TRUE, info = r)
  # Every DOI is in angle brackets, after a year, with no space inside.
  dois <- regmatches(text, gregexpr("<doi:[^>]*>", text))[[1]]
  expect_length(dois, length(refs))
  expect_false(any(grepl("[[:space:]]", dois)))
  expect_false(grepl("in press|forthcoming|submitted", text, ignore.case = TRUE))
  expect_false(grepl("follows the factor analytic and accuracy in parameter estimation traditions", text, fixed = TRUE))
})

# ---- one map, in SVG ------------------------------------------------------------------

test_that("the map of the five workspaces is one SVG file for the help page, the README and the articles", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  help_svg <- file.path(root, "man", "figures", "siever-map-workspaces.svg")
  article_svg <- file.path(root, "vignettes", "figures", "siever-map-workspaces.svg")
  expect_true(file.exists(help_svg))
  # Byte-identical copies, so the help page, the README and the articles
  # cannot drift apart.
  expect_identical(unname(tools::md5sum(help_svg)), unname(tools::md5sum(article_svg)))
  expect_false(file.exists(file.path(root, "man", "figures", "siever-map-workspaces.png")))
  rd <- paste(readLines(file.path(root, "man", "SIEVER-package.Rd"), warn = FALSE), collapse = " ")
  expect_match(rd, "\\figure{siever-map-workspaces.svg}", fixed = TRUE)
  expect_match(rd, "\"siever-map-workspaces.svg\", package = \"SIEVER\"", fixed = TRUE)
  expect_false(grepl("siever-map-workspaces.png", rd, fixed = TRUE))
  readme <- paste(readLines(file.path(root, "README.md"), warn = FALSE), collapse = " ")
  expect_match(readme, "(man/figures/siever-map-workspaces.svg)", fixed = TRUE)
  expect_false(grepl("siever-map-workspaces.png", readme, fixed = TRUE))
  # The high-resolution hex master is left out of the build; logo.svg (the
  # site logo pkgdown finds first) and logo.png (the README) are in it.
  ignore <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)
  ignored <- function(path) any(vapply(ignore, function(p) grepl(p, path, perl = TRUE, ignore.case = TRUE), logical(1)))
  expect_true(ignored("man/figures/logo-hires.png"))
  expect_false(ignored("man/figures/logo.svg"))
  expect_false(ignored("man/figures/logo.png"))
  expect_false(ignored("man/figures/siever-map-workspaces.svg"))
})

# ---- no DMAR capability probes -------------------------------------------------------

dmar_capability_probes <- c(
  "sieve_dmar_point_estimates_only", "sieve_dmar_rmsea_level_argument",
  "sieve_dmar_irt_rmsea_level_argument", "sieve_dmar_categorical_denominator",
  "sieve_irt_attribute_rmsea_level", "sieve_draft_model_implied"
)

test_that("SIEVER defines no DMAR capability probe and passes DMAR 1.1.0's arguments unconditionally", {
  ns <- asNamespace("SIEVER")
  for (probe in dmar_capability_probes) {
    expect_false(exists(probe, envir = ns, inherits = FALSE), info = probe)
  }
  # The arguments DMAR 1.1.0 takes are passed unconditionally.
  R <- stats::cor(sieve_hs9_fixture()$data[, sieve_hs9_fixture()$item_cols])
  expect_true(sieve_efa_point_call(3L, R = R, n = 301L, seed = 1L)$args$point_estimates_only)
  rt <- sieve_efa_dmar_route("ml", list(estimator = "ML", missing = "listwise"))
  expect_identical(sieve_efa_dmar_call(rt, 2L, data = sieve_hs9_fixture()$data[, sieve_hs9_fixture()$item_cols])$args$rmsea_conf_level, 0.90)
  expect_match(paste(sieve_irt_replay_code(c("x1", "x2", "x3"), "WLSMV"), collapse = "\n"),
    "metric = \"normal_ogive\", rmsea_conf_level = 0.9)", fixed = TRUE)
  sp <- sieve_reliability_spec(c("a", "b", "c"), rep("ordered", 3), ci_method = "none", denominator = "model_implied")
  expect_identical(sp$denominator, "model_implied")
  expect_identical(sieve_reliability_call(sp)$args$denominator, "model_implied")
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  src <- unlist(lapply(list.files(file.path(root, "R"), full.names = TRUE), readLines, warn = FALSE))
  code <- src[!grepl("^[[:space:]]*#", src)]
  expect_false(any(grepl("getNamespaceExports(\"DMAR\")", code, fixed = TRUE)))
  expect_false(any(grepl("formals(DMAR::", code, fixed = TRUE)))
  expect_false(any(grepl("formals(getExportedValue(\"DMAR\"", code, fixed = TRUE)))
})

test_that("no test or source file calls a DMAR capability probe", {
  # The check above reads the namespace, not the tests: a test that called
  # skip_if_not(sieve_dmar_categorical_denominator()) would stop its block
  # with "could not find function" wherever NOT_CRAN is true.
  files <- list.files(testthat::test_path(), pattern = "[.][Rr]$", full.names = TRUE)
  root <- compat_source_root()
  if (!is.null(root)) files <- c(files, list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE))
  expect_true(any(basename(files) == "test-public-surface.R"))
  calls <- character(0)
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    for (probe in dmar_capability_probes) {
      at <- grep(paste0("(^|[^[:alnum:]_.])", probe, "[[:space:]]*[(]"), lines, useBytes = TRUE)
      at <- at[!grepl("^[[:space:]]*#", lines[at], useBytes = TRUE)]
      calls <- c(calls, sprintf("%s:%d calls %s()", basename(f), at, rep(probe, length(at))))
    }
  }
  expect_identical(calls, character(0))
})

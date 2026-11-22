# The Standards crosswalk article (the crosswalk to the Standards for
# Educational and Psychological Testing). The article maps each SIEVER output to the
# chapters on validity, reliability/precision and fairness of the Standards
# (AERA, APA, & NCME, 2014), says what each output does not establish, and
# lists the evidence those chapters call for that SIEVER does not provide.
# These tests read the source tree, so they skip where the vignettes are not
# beside the tests (an installed package).

crosswalk_source <- function() {
  root <- compat_source_root()
  if (is.null(root)) return(NULL)
  rmd <- file.path(root, "vignettes", "standards-crosswalk.Rmd")
  list(root = root, rmd = rmd, text = if (file.exists(rmd)) readLines(rmd, warn = FALSE, encoding = "UTF-8") else NULL)
}

crosswalk_flat <- function(text) gsub("[[:space:]]+", " ", paste(text, collapse = " "))

# The lines of one "## " section, its heading excluded.
crosswalk_section <- function(text, heading) {
  start <- match(heading, text)
  if (is.na(start)) return(character(0))
  later <- which(grepl("^## ", text) & seq_along(text) > start)
  end <- if (length(later)) later[1L] - 1L else length(text)
  text[seq.int(start + 1L, end)]
}

test_that("the Standards crosswalk is a registered article, in the site's menu and index, and opened by name from the README, the user guide and ?SIEVER", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  expect_true(file.exists(src$rmd))
  skip_if(is.null(src$text), "the article is missing")
  header <- src$text[seq_len(min(10L, length(src$text)))]
  title <- sub('^title: "(.*)"$', "\\1", grep("^title:", header, value = TRUE))
  expect_identical(title, "SIEVER and the Standards for Educational and Psychological Testing (2014)")
  expect_true(any(grepl(sprintf("%%\\VignetteIndexEntry{%s}", title), header, fixed = TRUE)))
  expect_true(any(grepl("%\\VignetteEngine{knitr::rmarkdown}", header, fixed = TRUE)))
  expect_true(any(grepl("%\\VignetteEncoding{UTF-8}", header, fixed = TRUE)))
  expect_identical(unname(read.dcf(file.path(src$root, "DESCRIPTION"), fields = "VignetteBuilder")[1L, 1L]), "knitr")

  # Every article, this one included, is in the site's Articles menu and in
  # its article index, and nothing else is.
  articles <- sub("[.]Rmd$", "", list.files(file.path(src$root, "vignettes"), pattern = "[.]Rmd$"))
  expect_true("standards-crosswalk" %in% articles)
  yml <- readLines(file.path(src$root, "_pkgdown.yml"), warn = FALSE)
  menu <- unique(sub("^\\s*href: articles/(.*)[.]html$", "\\1", grep("^\\s*href: articles/", yml, value = TRUE)))
  expect_setequal(menu, articles)
  index <- yml[seq(grep("^articles:", yml), grep("^reference:", yml) - 1L)]
  listed <- trimws(sub("^\\s*-\\s*", "", grep("^\\s+-\\s+[a-z]", index, value = TRUE)))
  expect_setequal(listed, articles)
  expect_true(any(grepl(sprintf('- text: "%s"', title), yml, fixed = TRUE)))

  # The README and the help page open it by name, and the user guide's list
  # of the other articles opens every article but itself.
  call <- 'vignette("standards-crosswalk", package = "SIEVER")'
  expect_match(crosswalk_flat(readLines(file.path(src$root, "README.md"), warn = FALSE)), call, fixed = TRUE)
  expect_match(crosswalk_flat(readLines(file.path(src$root, "man", "SIEVER-package.Rd"), warn = FALSE)), call, fixed = TRUE)
  guide <- crosswalk_flat(readLines(file.path(src$root, "vignettes", "user-guide.Rmd"), warn = FALSE))
  for (name in setdiff(articles, "user-guide")) {
    expect_match(guide, sprintf('vignette("%s", package = "SIEVER")', name), fixed = TRUE, info = name)
  }
})

test_that("the crosswalk names the three foundational chapters, the evidence SIEVER does not provide, and no numbered standard", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  skip_if(is.null(src$text), "the article is missing")
  text <- src$text
  flat <- crosswalk_flat(text)
  for (heading in c(
    "## Validity (Chapter 1)",
    "## Reliability/Precision and Errors of Measurement (Chapter 2)",
    "## Fairness in Testing (Chapter 3)",
    "## Scores, Norms, Test Development, and Documentation",
    "## What SIEVER Does Not Provide",
    "## References"
  )) {
    expect_true(heading %in% text, info = heading)
  }
  # The outputs the article names, each in the chapter it serves.
  chapter <- function(heading) crosswalk_flat(crosswalk_section(text, heading))
  validity <- chapter("## Validity (Chapter 1)")
  for (output in c("content validity index", "Included items per construct", "parallel analysis", "confirmatory measurement model",
    "HTMT", "Fisher z interval", "validation reservation", "ECVI")) {
    expect_match(validity, output, fixed = TRUE, info = output)
  }
  reliability <- chapter("## Reliability/Precision and Errors of Measurement (Chapter 2)")
  for (output in c("Robust omega (observed denominator), the primary coefficient, with its percentile bootstrap interval",
    "coefficient H with its interval", "conditional standard error of theta", "Factor scores with an interval for each person",
    "alpha only on request")) {
    expect_match(reliability, output, fixed = TRUE, info = output)
  }
  fairness <- chapter("## Fairness in Testing (Chapter 3)")
  for (output in c("configural, thresholds for ordered items, metric, scalar, strict", "dMACS", "Alignment", "Longitudinal invariance")) {
    expect_match(fairness, output, fixed = TRUE, info = output)
  }
  expect_match(chapter("## Scores, Norms, Test Development, and Documentation"), "frozen calibration reference", fixed = TRUE)
  # What SIEVER does not provide.
  gaps <- chapter("## What SIEVER Does Not Provide")
  for (gap in c("Classification consistency and accuracy", "Score linking and equating", "Standard setting",
    "consequences of testing", "response processes", "test-retest", "Generalizability theory",
    "standard error of measurement of an observed composite", "Differential item functioning",
    "Differential prediction", "Accessibility, accommodations")) {
    expect_match(gaps, gap, fixed = TRUE, info = gap)
  }
  # The Standards are cited by chapter only: no standard number appears.
  expect_match(flat, paste(
    "American Educational Research Association, American Psychological Association, &",
    "National Council on Measurement in Education. (2014). *Standards for educational and psychological testing*."
  ), fixed = TRUE)
  expect_false(grepl("\\bStandards? [0-9]+[.][0-9]+", flat, perl = TRUE))
  # Every work cited in the text is in the references, and every reference
  # is cited, matched by first author (AERA for the Standards).
  body <- crosswalk_flat(text[seq_len(match("## References", text) - 1L)])
  groups <- sub("^[(](.*)[)]$", "\\1", regmatches(body, gregexpr("[(][^()]*[0-9]{4}[^()]*[)]", body))[[1]])
  pieces <- trimws(unlist(strsplit(groups, ";", fixed = TRUE)))
  pieces <- pieces[grepl("^[A-Z][A-Za-z-]+, .*[0-9]{4}$", pieces) | grepl("^[A-Z][A-Za-z-]+ & .*[0-9]{4}$", pieces)]
  cited <- unique(sub("^([A-Z][A-Za-z-]+).*$", "\\1", pieces))
  expect_true(length(cited) >= 15L)
  section <- crosswalk_section(text, "## References")
  starts <- which(nzchar(section) & c(TRUE, !nzchar(section[-length(section)])))
  first_authors <- sub(",.*$", "", section[starts])
  first_authors[first_authors == "American Educational Research Association"] <- "AERA"
  expect_setequal(cited, first_authors)
})

test_that("every function the crosswalk names exists, and its experimental marks agree with the help", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  skip_if(is.null(src$text), "the article is missing")
  flat <- crosswalk_flat(src$text)
  ns <- readLines(file.path(src$root, "NAMESPACE"), warn = FALSE)
  exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
  named <- unique(regmatches(flat, gregexpr("\\bsieve_[A-Za-z0-9_]+(?=\\(\\))|\\bsieve_[A-Za-z0-9_]+(?=\\(\")", flat, perl = TRUE))[[1]])
  expect_true(length(named) >= 30L)
  expect_identical(setdiff(named, exports), character(0))
  dmar <- unique(sub("^DMAR::", "", regmatches(flat, gregexpr("DMAR::[A-Za-z0-9_.]+(?=\\(\\))", flat, perl = TRUE))[[1]]))
  expect_true(length(dmar) >= 5L)
  expect_identical(setdiff(dmar, getNamespaceExports("DMAR")), character(0))
  # The services it runs by name are sieve_analyze()'s.
  assignment <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) && identical(e[[2L]], as.name("services")),
    as.list(body(sieve_analyze)))
  expect_length(assignment, 1L)
  services <- names(eval(assignment[[1L]][[3L]]))
  run <- unique(sub('^sieve_analyze\\("([a-z_]+)"\\)$', "\\1", regmatches(flat, gregexpr('sieve_analyze\\("[a-z_]+"\\)', flat))[[1]]))
  expect_true(length(run) >= 6L)
  expect_identical(setdiff(run, services), character(0))
  # A function the article marks experimental is one whose help says so,
  # and an experimental function the article names carries the mark, when
  # the help pages carry the sentence.
  marked <- unique(sub("^`(sieve_[A-Za-z0-9_]+)\\(\\)` \\(experimental\\)$", "\\1",
    regmatches(flat, gregexpr("`sieve_[A-Za-z0-9_]+\\(\\)` \\(experimental\\)", flat))[[1]]))
  expect_true(length(marked) >= 4L)
  rd <- list.files(file.path(src$root, "man"), pattern = "[.]Rd$", full.names = TRUE)
  experimental <- unique(unlist(lapply(rd, function(f) {
    lines <- readLines(f, warn = FALSE)
    if (!any(grepl("Experimental: the arguments", lines, fixed = TRUE))) return(character(0))
    sub("^\\\\alias\\{(.*)\\}$", "\\1", grep("^\\\\alias\\{", lines, value = TRUE))
  })))
  if (length(experimental)) {
    expect_identical(setdiff(marked, experimental), character(0))
    expect_identical(setdiff(intersect(named, experimental), marked), character(0))
  }
})

test_that("the crosswalk states the interval levels SIEVER uses by default", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  skip_if(is.null(src$text), "the article is missing")
  withr::local_options(sieve.rmsea_conf_level = NULL)
  flat <- crosswalk_flat(src$text)
  expect_match(flat, sprintf("Every interval is at the %s%% level by default (`sieve_default_conf_level()`)",
    format(100 * sieve_default_conf_level())), fixed = TRUE)
  # The unbiased SRMR's interval is at the RMSEA level too (its default in
  # sieve_fit_usrmr()), and the article says so.
  expect_identical(formals(sieve_fit_usrmr)$conf_level, quote(sieve_default_rmsea_conf_level()))
  expect_match(flat, sprintf(paste0("except the RMSEA's and that of the unbiased SRMR beside the SRMR, which are at %s%% ",
    "(`sieve_default_rmsea_conf_level()`"), format(100 * sieve_default_rmsea_conf_level())), fixed = TRUE)
  # Each RMSEA the tables name carries its interval at that level.
  rmsea <- regmatches(flat, gregexpr("RMSEA with its [0-9]+% interval", flat))[[1]]
  expect_true(length(rmsea) >= 3L)
  expect_identical(unique(rmsea), sprintf("RMSEA with its %s%% interval", format(100 * sieve_default_rmsea_conf_level())))
})

test_that("the crosswalk and ?SIEVER read HTMT against no value and give PUC no interval, as the Validity and Bifactor pages do", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  skip_if(is.null(src$text), "the article is missing")
  flat <- crosswalk_flat(src$text)
  # There is no HTMT threshold input and no column that reads each bound
  # against one (SIEVER applies no cutoff in output), and the bounds table
  # has no threshold.
  expect_identical(names(formals(sieve_htmt_bounds_table)), "h")
  expect_false(grepl("stated value", flat, fixed = TRUE))
  expect_match(flat, paste("An upper bound well below 1 is evidence that two constructs separate,",
    "not proof of discriminant validity; SIEVER applies no cutoff to HTMT."), fixed = TRUE)
  # PUC is fixed by the model, so the bootstrap gives it no interval.
  expect_match(flat, paste("ECV, omega, omegaH, omegaHS and H with bootstrap intervals, and PUC, which the",
    "model's structure fixes and so has no interval"), fixed = TRUE)
  expect_false(grepl("PUC and H, with bootstrap intervals", flat, fixed = TRUE))
  help <- crosswalk_flat(readLines(file.path(src$root, "man", "SIEVER-package.Rd"), warn = FALSE))
  expect_false(grepl("HTMT read against", help, fixed = TRUE))
  expect_match(help, "HTMT with its one-sided upper bound, criterion validity read against the predictions you state", fixed = TRUE)
  expect_match(help, "(ECV and omegaH with their bootstrap intervals, and PUC, which the model's structure fixes)", fixed = TRUE)
  expect_false(grepl("(ECV, omegaH, PUC) with their bootstrap intervals", help, fixed = TRUE))
})

test_that("the crosswalk builds fast and follows the house style: no evaluated code, whole tables, APA title case, no em dash", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  skip_if(is.null(src$text), "the article is missing")
  text <- src$text
  # No model is fit when the article is built: the only chunk evaluated is
  # the setup chunk, which sets knitr's options.
  chunks <- grep("^```\\{r", text, value = TRUE)
  evaluated <- chunks[!grepl("eval\\s*=\\s*FALSE", chunks)]
  expect_identical(evaluated, "```{r setup, include = FALSE}")
  setup_body <- text[seq(match("```{r setup, include = FALSE}", text) + 1L, length.out = 1L)]
  expect_identical(setup_body, 'knitr::opts_chunk$set(collapse = TRUE, comment = "#>")')
  # Every row of a table has as many cells as its header.
  is_row <- grepl("^\\|.*\\|$", text)
  blocks <- split(which(is_row), cumsum(!is_row)[is_row])
  expect_true(length(blocks) >= 5L)
  for (rows in blocks) {
    cells <- vapply(text[rows], function(line) lengths(regmatches(line, gregexpr("|", line, fixed = TRUE))), integer(1))
    expect_true(length(rows) >= 3L)
    expect_identical(unname(cells), rep(cells[[1L]], length(rows)), info = text[rows[1L]])
  }
  # No em dash, and no "---" outside the front matter's delimiters and the
  # tables' header rules.
  expect_false(any(grepl("\u2014", text, fixed = TRUE)))
  delimiters <- which(text == "---")
  expect_identical(delimiters[1:2], c(1L, 8L))
  rule <- grepl("^\\|(:?-+:?\\|)+$", text)
  rest <- text[-c(delimiters[1:2], which(rule))]
  expect_false(any(grepl("---", rest, fixed = TRUE)))
  # Headings in APA title case: the first word, and every word of four
  # letters or more (each part of a slashed or hyphenated word), capitalized.
  headings <- sub("^#+ ", "", grep("^#{1,6} ", text, value = TRUE))
  expect_true(length(headings) >= 8L)
  for (heading in headings) {
    words <- unlist(strsplit(gsub("[(),:]", " ", heading), "[[:space:]/-]+"))
    words <- words[nzchar(words) & grepl("^[A-Za-z]", words)]
    expect_match(words[1L], "^[A-Z]", info = heading)
    long <- words[nchar(words) >= 4L]
    expect_true(all(grepl("^[A-Z]", long)), info = heading)
  }
})

# The cells of a Markdown table, one character vector per row, for the table
# whose header row matches `header` (the header and rule rows excluded).
crosswalk_tables <- function(text, header) {
  is_row <- grepl("^\\|.*\\|$", text)
  starts <- which(is_row & grepl(header, text, perl = TRUE))
  lapply(starts, function(start) {
    end <- start
    while (end < length(text) && is_row[end + 1L]) end <- end + 1L
    rows <- text[seq.int(start + 2L, end)]
    lapply(rows, function(line) trimws(strsplit(sub("^\\|(.*)\\|$", "\\1", line), "|", fixed = TRUE)[[1L]]))
  })
}

# A place named in the application, its parenthetical remarks (the tabs in
# quotation marks) removed, innermost first.
crosswalk_strip_parens <- function(x) {
  repeat {
    y <- gsub("\\s*\\([^()]*\\)", "", x)
    if (identical(y, x)) return(trimws(y))
    x <- y
  }
}

test_that("every place the crosswalk names in the application gives its workspace first, as the application's own messages do", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  skip_if(is.null(src$text), "the article is missing")
  # Each page as the application names it (sieve_page_path_text(): the
  # workspace, an arrow, the page). Scoring and Norms is a tab of Reliability
  # and is named as one, in quotation marks.
  ids <- setdiff(names(sieve_page_catalog()), "scoring")
  paths <- vapply(ids, sieve_page_path_text, character(1))
  expect_true(all(grepl("^(Study|Structure|Scores|Evidence|Report) → ", paths)))
  tables <- crosswalk_tables(src$text, "\\| Where \\|")
  expect_length(tables, 4L)
  # Where is the third column of each; the place in the application comes
  # before the semicolon that introduces the R function.
  places <- unlist(lapply(tables, function(rows) vapply(rows, function(cells) sub(";.*$", "", cells[[3L]]), character(1))))
  expect_true(length(places) >= 30L)
  named <- trimws(unlist(strsplit(crosswalk_strip_parens(places), ",", fixed = TRUE)))
  # The one place outside the workspaces is the project bar's assistant.
  outside <- named[!named %in% paths]
  expect_identical(outside, "The project bar's Scientific task assistant")
  # Every workspace with an output in the tables is named.
  expect_setequal(unique(sub(" → .*$", "", named[named %in% paths])), c("Study", "Structure", "Scores", "Evidence", "Report"))
})

test_that("the crosswalk's table of workspaces is the application's: each workspace, its question and its pages in order", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  skip_if(is.null(src$text), "the article is missing")
  reading <- crosswalk_section(src$text, "## How to Read the Tables")
  expect_true(any(grepl("workspace", reading, fixed = TRUE)))
  rows <- crosswalk_tables(reading, "^\\| Workspace \\| The question it answers \\| Its pages \\|$")
  expect_length(rows, 1L)
  rows <- rows[[1L]]
  workspaces <- sieve_workspaces()
  catalog <- sieve_page_catalog()
  expect_identical(vapply(rows, `[[`, character(1), 1L), unname(vapply(workspaces, `[[`, character(1), "label")))
  expect_identical(vapply(rows, `[[`, character(1), 2L), unname(vapply(workspaces, `[[`, character(1), "question")))
  for (i in seq_along(workspaces)) {
    listed <- trimws(strsplit(crosswalk_strip_parens(rows[[i]][[3L]]), ",", fixed = TRUE)[[1L]])
    expected <- unname(vapply(workspaces[[i]]$pages, function(id) catalog[[id]]$label, character(1)))
    expect_identical(listed, expected, info = workspaces[[i]]$label)
  }
  # Scoring and Norms is named where it sits, within Reliability.
  scores <- rows[[match("Scores", vapply(rows, `[[`, character(1), 1L))]][[3L]]
  expect_match(scores, "Reliability (with its Scoring and Norms tab)", fixed = TRUE)
  expect_identical(sieve_page_path_text("scoring"), "Scores → Reliability → Scoring and Norms")
})

test_that("the content validity statistics are credited to their own sources, in the crosswalk and in the user guide", {
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  skip_if(is.null(src$text), "the article is missing")
  lawshe <- "Lawshe, C. H. (1975). A quantitative approach to content validity. *Personnel Psychology, 28*(4), 563--575."
  owen <- paste(
    "Polit, D. F., Beck, C. T., & Owen, S. V. (2007). Is the CVI an acceptable indicator of content validity?",
    "Appraisal and recommendations. *Research in Nursing & Health, 30*(4), 459--467. https://doi.org/10.1002/nur.20199"
  )
  # The crosswalk: the modified kappa is Polit, Beck, and Owen's (2007), the
  # content validity ratio Lawshe's (1975), and the CVI family Lynn's (1986)
  # and Polit and Beck's (2006), as DMAR::content_validity_index() and the
  # Content Validity page credit them.
  content <- grep("^\\| Test content \\| An expert panel's relevance ratings", src$text, value = TRUE)
  expect_length(content, 1L)
  expect_match(content, "S-CVI/Ave and S-CVI/UA (Lynn, 1986; Polit & Beck, 2006)", fixed = TRUE)
  expect_match(content, "the modified kappa, which corrects the I-CVI for chance agreement (Polit, Beck, & Owen, 2007)", fixed = TRUE)
  expect_match(content, "the content validity ratio (Lawshe, 1975)", fixed = TRUE)
  references <- crosswalk_flat(crosswalk_section(src$text, "## References"))
  expect_match(references, lawshe, fixed = TRUE)
  expect_match(references, owen, fixed = TRUE)
  # The user guide's account of the Content Validity page.
  guide_text <- readLines(file.path(src$root, "vignettes", "user-guide.Rmd"), warn = FALSE, encoding = "UTF-8")
  guide <- crosswalk_flat(guide_text)
  expect_match(guide, "modified kappa, which corrects the I-CVI for chance agreement (Polit, Beck, & Owen, 2007), Lawshe's (1975) content validity ratio (CVR)", fixed = TRUE)
  guide_references <- crosswalk_flat(crosswalk_section(guide_text, "## References"))
  expect_match(guide_references, lawshe, fixed = TRUE)
  expect_match(guide_references, owen, fixed = TRUE)
  # The application credits the same two sources on the page.
  module <- crosswalk_flat(readLines(file.path(src$root, "R", "mod_content.R"), warn = FALSE))
  expect_match(module, "Polit, Beck & Owen, 2007", fixed = TRUE)
  expect_match(module, "Lawshe's (1975)", fixed = TRUE)
})

test_that("every registry work the user guide cites by author and year has an entry in its References", {
  # The guide cites registry works by author and year, among them
  # Maydeu-Olivares (2017) and Maydeu-Olivares, Shi and Rosseel (2018) for
  # the unbiased SRMR, Schomaker and Heumann (2018), Bartlett and Hughes
  # (2020), and the network works of Epskamp and of van Bork et al. (2021);
  # each needs an entry in its References. A work is keyed by its first
  # author's surname and its year.
  src <- crosswalk_source()
  skip_if(is.null(src), "the source tree is not beside the tests")
  guide <- readLines(file.path(src$root, "vignettes", "user-guide.Rmd"), warn = FALSE, encoding = "UTF-8")
  start <- match("## References", guide)
  expect_false(is.na(start))
  body <- gsub("et al[.]", "et al", crosswalk_flat(guide[seq_len(start - 1L)]))
  section <- crosswalk_section(guide, "## References")
  entries <- trimws(vapply(split(section, cumsum(!nzchar(trimws(section)))), crosswalk_flat, character(1)))
  entries <- entries[nzchar(entries)]
  refs <- sieve_method_references()
  refs <- refs[refs$kind == "method" & !is.na(refs$doi) & nzchar(refs$doi), ]
  key <- unique(data.frame(
    first = sub("^([^,]+),.*$", "\\1", refs$citation),
    year = sub("^[^(]*[(]([0-9]{4})[a-z]?[)].*$", "\\1", refs$citation)
  ))
  key <- key[grepl("^[0-9]{4}$", key$year), ]
  expect_gt(nrow(key), 50L)
  cited <- vapply(seq_len(nrow(key)), function(i) {
    grepl(sprintf("\\Q%s\\E[^;()]{0,60}?[ (]%s[a-z]?\\b", key$first[i], key$year[i]), body, perl = TRUE)
  }, logical(1))
  listed <- vapply(seq_len(nrow(key)), function(i) {
    any(grepl(sprintf("^\\Q%s\\E,.*[(]%s[a-z]?[)]", key$first[i], key$year[i]), entries, perl = TRUE))
  }, logical(1))
  # The key finds the works the References hold.
  expect_gt(sum(cited & listed), 40L)
  expect_identical(sprintf("%s (%s)", key$first, key$year)[cited & !listed], character(0))
  # The two works the unbiased SRMR rests on, as the registry gives them.
  flat <- crosswalk_flat(section)
  expect_match(flat, "Maydeu-Olivares, A. (2017). Assessing the size of model misfit in structural equation models. *Psychometrika, 82*(3), 533--558. https://doi.org/10.1007/s11336-016-9552-7", fixed = TRUE)
  expect_match(flat, "Maydeu-Olivares, A., Shi, D., & Rosseel, Y. (2018). Assessing fit in structural equation models: A Monte-Carlo evaluation of RMSEA versus SRMR confidence intervals and tests of close fit. *Structural Equation Modeling, 25*(3), 389--402. https://doi.org/10.1080/10705511.2017.1389611", fixed = TRUE)
})

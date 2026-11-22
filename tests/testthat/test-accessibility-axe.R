# An automated accessibility scan with axe-core: each of the five workspaces,
# with the bundled example loaded, at 1280 x 800 and at 760 x 900, checked
# against the WCAG 2.0, 2.1 and 2.2 A and AA rules axe-core implements. The
# acceptance is no violation of serious or critical impact; lesser ones are
# written to the folder named by SIEVER_BROWSER_EVIDENCE when it is set.
#
# axe-core (Deque Systems, MPL-2.0) is not a dependency of SIEVER and the
# tests never download it. The scan runs when its browser build is on this
# machine: the file named by SIEVER_AXE_CORE, or tests/testthat/vendor/
# axe.min.js. Otherwise the test skips and says what is missing.
#
# Author: Ken Kelley

siever_axe_core <- function() {
  candidates <- c(Sys.getenv("SIEVER_AXE_CORE", unset = ""), testthat::test_path("vendor", "axe.min.js"))
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found)) found[[1L]] else NULL
}

test_that("axe-core finds no serious or critical violation in the five workspaces at 1280 and 760 px", {
  skip_if_no_browser()
  axe <- siever_axe_core()
  skip_if(is.null(axe), paste(
    "axe-core is not available: no file at SIEVER_AXE_CORE and no tests/testthat/vendor/axe.min.js.",
    "Set SIEVER_AXE_CORE to a local copy of axe-core's axe.min.js to run the scan; the tests do not download it."
  ))
  source <- paste(readLines(axe, warn = FALSE), collapse = "\n")
  scan <- paste0(
    "axe.run(document, {runOnly: {type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21a', 'wcag21aa', 'wcag22aa']},",
    " resultTypes: ['violations']}).then(function(r){return r.violations.map(function(v){",
    "return {id: v.id, impact: v.impact || '', nodes: v.nodes.length,",
    " targets: v.nodes.slice(0, 5).map(function(n){return n.target.join(' ');}).join('; ')};});})"
  )
  evidence <- Sys.getenv("SIEVER_BROWSER_EVIDENCE")
  for (size in list(c(1280L, 800L), c(760L, 900L))) {
    compat_with_app(width = size[[1L]], height = size[[2L]], load_example = TRUE, function(app) {
      for (ws in names(sieve_workspaces())) {
        page <- sieve_workspaces()[[ws]]$pages[[1L]]
        siever_open_tab(app, page)
        if (!isTRUE(app$get_js("typeof window.axe === 'object'"))) app$run_js(source)
        found <- app$get_js(scan)
        rows <- if (length(found)) do.call(rbind, lapply(found, as.data.frame)) else
          data.frame(id = character(), impact = character(), nodes = integer(), targets = character())
        if (nzchar(evidence) && nrow(rows)) {
          dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
          utils::write.csv(rows, file.path(evidence, sprintf("axe-%s-%d.csv", ws, size[[1L]])), row.names = FALSE)
        }
        severe <- rows[rows$impact %in% c("serious", "critical"), , drop = FALSE]
        expect_identical(
          sprintf("%s (%s, %d nodes: %s)", severe$id, severe$impact, as.integer(severe$nodes), severe$targets),
          character(0),
          info = sprintf("%s workspace (%s page) at %d px", ws, page, size[[1L]])
        )
      }
    })
  }
})

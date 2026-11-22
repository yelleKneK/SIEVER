# The Update button when a component's duration is unknown: the label
# states the known part as a floor and names how many components have no
# estimate, and the status line names them; it never passes a partial sum
# off as the whole.
#
# Author: Ken Kelley

pu_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}

test_that("the Update label states a floor and names the component whose duration is unknown", {
  expect_identical(sieve_update_duration_text(8, 0L, 2L), "about 8 seconds")
  expect_identical(sieve_update_duration_text(NA_real_, 2L, 2L), "unknown time")
  expect_identical(sieve_update_duration_text(NA_real_, 1L, 1L), "unknown time")
  expect_identical(sieve_update_duration_text(3, 1L, 2L), "at least 3 seconds; one component of unknown duration")
  expect_identical(sieve_update_duration_text(62, 2L, 4L), "at least 65 seconds; 2 components of unknown duration")
  expect_identical(sieve_update_duration_text(0.5, 1L, 2L), "under 2 seconds plus one component of unknown duration")

  entry <- function(title, seconds, sentence = NULL) {
    sieve_update_entry(title, "stale", sentence = sentence, seconds = seconds, run = function() NULL)
  }
  shiny::testServer(
    function(input, output, session) {
      entries <- shiny::reactiveVal(list())
      wire <- sieve_update_wire(input, session, function() entries())
    },
    {
      entries(list(
        cfa = entry("Correlated-factors model", 3, "an item was excluded"),
        htmt = entry("HTMT matrix", NA_real_, "an item was excluded")
      ))
      expect_identical(wire$seconds(), 3)
      expect_identical(wire$label(), "Update (at least 3 seconds; one component of unknown duration)")
      line <- pu_text(wire$line())
      expect_match(line, "The duration of the HTMT matrix is unknown and is not in the estimate.", fixed = TRUE)
      expect_match(line, "Update (at least 3 seconds; one component of unknown duration)", fixed = TRUE)
      # Every component unknown.
      entries(list(htmt = entry("HTMT matrix", NA_real_), cv = entry("Cross-validation", NA_real_)))
      expect_identical(wire$label(), "Update (unknown time)")
      expect_match(pu_text(wire$line()), "The duration of the HTMT matrix and cross-validation is unknown", fixed = TRUE)
      # Every component known: the label is the rounded sum, and the line
      # says nothing about unknown durations.
      entries(list(cfa = entry("Correlated-factors model", 3), htmt = entry("HTMT matrix", 5)))
      expect_identical(wire$label(), "Update (about 8 seconds)")
      expect_false(grepl("unknown", pu_text(wire$line()), fixed = TRUE))
    }
  )
})

# Inspect recorded code in replay regression tests.
sieve_repro_reads <- function(code) {
  # A logged step may be one string holding several lines, so split before
  # matching: the anchors below are line anchors, not string anchors.
  body <- unlist(strsplit(as.character(code), "\n", fixed = TRUE))
  body <- body[!grepl("^\\s*#", body)]
  pat <- function(nm) sprintf("(^|[^A-Za-z0-9._])%s([^A-Za-z0-9._]|$)", nm)
  asg <- function(nm) sprintf("^\\s*%s\\s*(<-|=[^=])", nm)
  list(
    reads = function(nm) {
      length(body) > 0L &&
        any(grepl(pat(nm), body) & !grepl(asg(nm), body))
    },
    creates = function(nm) {
      length(body) > 0L && any(grepl(asg(nm), body))
    }
  )
}

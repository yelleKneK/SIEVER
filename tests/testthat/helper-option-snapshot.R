# The SIEVER options as they were when this test run started. A test that
# sets one on the wrong frame (for example inside a shiny::testServer()
# block, whose frame never ends) leaves it set for every later file of the
# run; test-zz-option-hygiene.R, which runs last, reports it.
siever_options_at_start <- local({
  o <- options()
  o <- o[grep("^sieve(r)?[.]", names(o))]
  o[order(names(o))]
})
# The search path as it was when this test run started. A function that
# attaches a package and leaves it attached changes the user's session as
# library() would (sieve_mi_model(), for one, must leave lavaan and
# lavaan.mi as it found them); test-zz-option-hygiene.R reports it.
siever_search_at_start <- search()

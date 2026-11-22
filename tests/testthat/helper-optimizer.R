# Count the live optimizer plan without enumerating candidates.
siever_test_optimizer_total <- function(meta, max_drop, min_keep, min_facet = NULL,
                                        must_keep = NULL, undersized = "hold") {
  checked <- sieve_capture(sieve_optimizer_constraints(max_drop = max_drop,
    min_keep = min_keep, min_facet = min_facet, must_keep = must_keep, undersized = undersized))
  if (!checked$ok) return(0)
  plan <- sieve_optimizer_plan(meta, checked$value)
  if (is.null(plan$error)) plan$total else 0
}

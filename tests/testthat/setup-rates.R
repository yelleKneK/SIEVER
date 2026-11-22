# SIEVER records the seconds it measures per unit of work for the whole R
# process (sieve_rate_note()), and its waits read them: a page whose
# standard estimate is two seconds or less runs by itself unless this
# machine's measured estimate is above six (sieve_runs_by_itself()), and
# the EFA tab fits a solution in the session when its measured estimate is
# two seconds or less and in a background process otherwise. On a busy or
# slow machine the times measured by one test, or by the fits a test makes
# while setting up its study, would move a later page to its button or to a
# background process. So in this test run SIEVER records no measured rate,
# and every wait is at the standard rates: what a page does depends on the
# size of its problem, not on this machine's speed at that moment. A test
# of the measured rates calls local_measured_rates(), which records them as
# SIEVER does, from an empty store, for that test alone (helper-rates.R).
sieve_rate_reset()
testthat::local_mocked_bindings(
  sieve_rate_note = function(kind, seconds, units) invisible(SIEVER:::sieve_rate(kind)),
  .package = "SIEVER", .env = testthat::teardown_env()
)

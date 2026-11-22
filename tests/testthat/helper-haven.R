# The suite runs with haven loaded, as any session that has read a .sav or
# .dta file does: haven's methods are then registered, and as.numeric() or
# as.double() on a labelled column refuses (vctrs) where it would otherwise
# pass. A conversion that skips sieve_as_number() then fails here instead of
# in a researcher's session, whatever order the tests load packages in.
if (requireNamespace("haven", quietly = TRUE)) loadNamespace("haven")

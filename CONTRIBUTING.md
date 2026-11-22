# Contributing to SIEVER

Describe the scientific target and a reproducible example before changing a
calculation. Preserve full precision and stable item and respondent
identities. Use a published methodological source or an independent numerical
oracle. Do not relax a tolerance merely to make a failing test pass.

Keep inference services separate from Shiny renderers. A new analysis needs a
headless public contract, an explicit sampling and missing-data
specification, runnable recorded code, and a reason when an estimate is
unavailable. Use optional dependencies only behind an availability check.

Before proposing a change, run the test files that exercise what it touches
(search `tests/testthat` for the functions, files and pages it changes;
README, vignette and `_pkgdown.yml` edits have tests too) and
`R CMD check --as-cran` on a built archive. Report failures, warnings, skips
and the environment. Use synthetic or publicly redistributable examples;
never attach participant records or credentials.

Report bugs at https://github.com/yelleKneK/SIEVER/issues with a minimal
reproducible example, the output of `sessionInfo()`, and the engine identity
recorded in the project or script.

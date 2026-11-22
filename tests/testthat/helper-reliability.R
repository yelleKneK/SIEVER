# The label categorical omega carries for a requested denominator. DMAR's
# reliability_omega_categorical() (1.1.0, the version SIEVER requires) takes
# the denominator, so a request for the model-implied one (the polychoric
# correlations the one-factor model implies, Green & Yang, 2009) is honored.
categorical_omega_label <- function(denominator = "model_implied") {
  if (identical(denominator, "model_implied")) {
    "Omega (model-implied denominator: polychoric correlations implied by the one-factor model)"
  } else {
    "Robust omega (observed denominator: unrestricted polychoric correlations)"
  }
}

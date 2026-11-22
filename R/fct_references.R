# fct_references.R
# The methods registry: one row per method or piece of software SIEVER can
# cite, with the citation in APA form, its DOI or official URL, the claim the
# citation supports, and where it applies. A report cites only the methods
# that were actually run: each recorded analysis carries the keys of the
# methods its specification used (sieve_spec_methods(), written into the
# registry record by sieve_register_analysis()), and a bibliography is the
# deduplicated entries of the keys of the selected records plus the software
# they ran in, with the versions recorded. Every entry is verified against
# CrossRef (https://api.crossref.org/works/<doi>), the publisher's or
# CRAN's page, or, for the book chapter with no DOI record, the edited
# volume. An entry marked verified = FALSE is kept in the registry and is
# left out of every export.
#
# Shiny-free: the registry and the bibliography builder read no application
# state.

#' The Registry of Methods and Software SIEVER Can Cite
#'
#' One row per method or piece of software: a short key, its kind, the
#' citation in APA form, its DOI or official URL, the claim the citation
#' supports, where it applies, the software the method runs through in
#' SIEVER, and whether the entry was verified against CrossRef or the
#' publisher's page (an unverified entry is kept in the registry for
#' completeness and is excluded from every export). A software row's
#' citation carries the placeholders `VERSION` and `YEAR`, which
#' [sieve_method_bibliography()] fills with the version actually used.
#'
#' The registry is the source of every reference list SIEVER writes: a
#' report from recorded analyses cites the methods of the selected records
#' (see [sieve_method_bibliography()]), and the methods draft cites the
#' methods it computed with. A method is never cited because it exists in
#' the registry; it is cited because an analysis used it.
#'
#' @return A data frame with character columns `key`, `kind` (`"method"` or
#'   `"software"`), `citation`, `doi` (a DOI, or the official URL), `claim`,
#'   `applicability`, `software` (the packages the method runs through,
#'   comma separated, `NA` for a software row), and a logical column
#'   `verified`.
#' @author Ken Kelley
#' @examples
#' refs <- sieve_method_references()
#' refs[refs$key %in% c("omega_robust", "fiml"), c("key", "citation")]
#' table(refs$kind)
#' @export
sieve_method_references <- function() {
  # The text is marked UTF-8 whatever the session's locale: the citations
  # carry accented names, and a radix sort refuses text of unknown encoding
  # under a C locale (as R CMD build's vignette stage runs).
  row <- function(key, kind, citation, doi, claim, applicability, software = NA_character_, verified = TRUE) {
    data.frame(
      key = key, kind = kind, citation = enc2utf8(citation), doi = doi, claim = enc2utf8(claim),
      applicability = enc2utf8(applicability), software = software, verified = verified,
      stringsAsFactors = FALSE
    )
  }
  rows <- list(
    row("multiple_imputation", "method",
      "Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. Wiley. https://doi.org/10.1002/9780470316696",
      "10.1002/9780470316696", "Pooling complete-data estimates and their uncertainty over multiple imputations.",
      "The separate MI sensitivity analyses."),
    row("imputation_count", "method",
      "von Hippel, P. T. (2020). How many imputations do you need? A two-stage calculation using a quadratic rule. Sociological Methods & Research, 49(3), 699--718. https://doi.org/10.1177/0049124117747303",
      "10.1177/0049124117747303", "Plan M from the upper pilot FMI confidence bound and a target CV of standard errors.",
      "Adaptive count planning after the MI pilot."),
    row("bootstrap_mi", "method",
      "Schomaker, M., & Heumann, C. (2018). Bootstrap inference when using multiple imputation. Statistics in Medicine, 37(14), 2252--2266. https://doi.org/10.1002/sim.7654",
      "10.1002/sim.7654", "Bootstrap then impute, averaging within each replication before forming percentile limits.",
      "Categorical omega under MAR.", "DMAR,mice"),
    row("bootstrap_mi_compatibility", "method",
      "Bartlett, J. W., & Hughes, R. A. (2020). Bootstrap inference for multiple imputation under uncongeniality and misspecification. Statistical Methods in Medical Research, 29(12), 3533--3546. https://doi.org/10.1177/0962280220932189",
      "10.1177/0962280220932189", "Bootstrap-then-imputation inference under uncongenial imputation and analysis models.",
      "Categorical omega under MAR.", "DMAR,mice"),
    row("mice", "software",
      "van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate imputation by chained equations in R. Journal of Statistical Software, 45(3), 1--67. https://doi.org/10.18637/jss.v045.i03 (mice version VERSION)",
      "10.18637/jss.v045.i03", "Chained-equations imputation using type-appropriate conditional models.",
      "Ordered-item MI sensitivity analysis.", "mice"),
    row("lavaan.mi", "software",
      "Jorgensen, T. D., & Rosseel, Y. (YEAR). lavaan.mi: Fit structural equation models to multiply imputed data (version VERSION). https://tdjorgensen.github.io/lavaan.mi/",
      "https://tdjorgensen.github.io/lavaan.mi/", "WLSMV fits across completed data sets and pooled estimates with t intervals.",
      "Ordered-item MI sensitivity analysis.", "lavaan.mi"),
    # ---- the measurement model and its estimators ---------------------------
    row(
      "cfa", "method",
      "J\u00f6reskog, K. G. (1969). A general approach to confirmatory maximum likelihood factor analysis. Psychometrika, 34(2), 183--202. https://doi.org/10.1007/BF02289343",
      "10.1007/BF02289343",
      "Confirmatory factor analysis by maximum likelihood: a measurement model that assigns each item to its construct and estimates loadings, residual variances and factor correlations.",
      "Any confirmatory measurement model (correlated factors, higher order, facets) on the CFA, SEM, Validity and Bifactor pages.",
      "lavaan"
    ),
    row(
      "mlr", "method",
      "Yuan, K.-H., & Bentler, P. M. (2000). Three likelihood-based methods for mean and covariance structure analysis with nonnormal missing data. Sociological Methodology, 30(1), 165--200. https://doi.org/10.1111/0081-1750.00078",
      "10.1111/0081-1750.00078",
      "Robust maximum likelihood (MLR): maximum likelihood estimates with sandwich standard errors and a scaled test statistic for nonnormal, possibly incomplete, continuous data.",
      "Every fit whose estimator is MLR (the Estimator control's choice for markedly nonnormal continuous items), including the MLR (sandwich) SE interval of model-implied omega.",
      "lavaan"
    ),
    row(
      "ml", "method",
      "Curran, P. J., West, S. G., & Finch, J. F. (1996). The robustness of test statistics to nonnormality and specification error in confirmatory factor analysis. Psychological Methods, 1(1), 16--29. https://doi.org/10.1037/1082-989X.1.1.16",
      "10.1037/1082-989X.1.1.16",
      "Normal-theory maximum likelihood standard errors and the likelihood ratio test are accurate under multivariate normality and are distorted by nonnormality, which is why robust ML (MLR) is the choice for markedly nonnormal items.",
      "Every fit whose estimator is ML (ML with FIML, SIEVER's and DMAR's default for continuous items, or ML on the complete cases), where the normal-theory statistics are reported as such.",
      "lavaan"
    ),
    row(
      "wlsmv", "method",
      "Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of alternative methods of estimation for confirmatory factor analysis with ordinal data. Psychological Methods, 9(4), 466--491. https://doi.org/10.1037/1082-989X.9.4.466",
      "10.1037/1082-989X.9.4.466",
      "Weighted least squares with mean and variance adjusted statistics (WLSMV) on polychoric correlations for ordered items, and its behavior relative to full weighted least squares.",
      "Every fit whose estimator is WLSMV (SIEVER's default when any item is ordered or binary).",
      "lavaan"
    ),
    row(
      "categorical_vs_continuous", "method",
      "Rhemtulla, M., Brosseau-Liard, P. \u00c9., & Savalei, V. (2012). When can categorical variables be treated as continuous? A comparison of robust continuous and categorical SEM estimation methods under suboptimal conditions. Psychological Methods, 17(3), 354--373. https://doi.org/10.1037/a0029315",
      "10.1037/a0029315",
      "When ordered items may be treated as continuous under robust ML and when the categorical estimator is preferable: the number of categories and the shape of the item distributions decide.",
      "Cited with WLSMV, and wherever the item treatment (continuous or ordered) is a reported choice.",
      "lavaan"
    ),
    row(
      "fiml", "method",
      "Enders, C. K., & Bandalos, D. L. (2001). The relative performance of full information maximum likelihood estimation for missing data in structural equation models. Structural Equation Modeling, 8(3), 430--457. https://doi.org/10.1207/S15328007SEM0803_5",
      "10.1207/S15328007SEM0803_5",
      "Full information maximum likelihood for incomplete data in structural equation models, compared with listwise and pairwise deletion and similar response pattern imputation.",
      "Every fit whose missing data treatment is FIML (MLR and ML with FIML).",
      "lavaan"
    ),
    # ---- fit indices ----------------------------------------------------------
    row(
      "fit_indices", "method",
      "Hu, L.-T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling, 6(1), 1--55. https://doi.org/10.1080/10705519909540118",
      "10.1080/10705519909540118",
      "The conventional cutoffs for the RMSEA, CFI, TLI and SRMR, derived for a given class of misspecified models.",
      "Every fitted model whose fit indices are reported.",
      "lavaan"
    ),
    row(
      "fit_indices_caution", "method",
      "Marsh, H. W., Hau, K.-T., & Wen, Z. (2004). In search of golden rules: Comment on hypothesis-testing approaches to setting cutoff values for fit indexes and dangers in overgeneralizing Hu and Bentler's (1999) findings. Structural Equation Modeling, 11(3), 320--341. https://doi.org/10.1207/s15328007sem1103_2",
      "10.1207/s15328007sem1103_2",
      "The cutoffs of Hu and Bentler (1999) are not golden rules: they depend on the model and the data, so fit is judged with the residuals and the substantive model, not by thresholds alone.",
      "Every fitted model whose fit indices are reported.",
      "lavaan"
    ),
    row(
      "rmsea_small_df", "method",
      "Kenny, D. A., Kaniskan, B., & McCoach, D. B. (2015). The performance of RMSEA in models with small degrees of freedom. Sociological Methods & Research, 44(3), 486--507. https://doi.org/10.1177/0049124114543236",
      "10.1177/0049124114543236",
      "With few degrees of freedom the RMSEA and its confidence interval too often indicate poor fit for a correctly specified model, especially in small samples.",
      "A fitted model with few degrees of freedom (SIEVER cites it below ten).",
      "lavaan"
    ),
    # The unbiased SRMR and its interval (DMAR::sem_residual_fit(), beside
    # the SRMR of the CFA and SEM pages and the methods draft). The entries
    # follow the typeset article of Maydeu-Olivares, Shi and Rosseel (2018)
    # (its first page gives the journal, volume, pages and DOI; its
    # reference list gives Maydeu-Olivares, 2017, as Psychometrika, 82,
    # 533--558, doi:10.1007/s11336-016-9552-7) and the references of
    # DMAR's ?sem_residual_fit, which give the issue numbers.
    row(
      "usrmr", "method",
      "Maydeu-Olivares, A. (2017). Assessing the size of model misfit in structural equation models. Psychometrika, 82(3), 533--558. https://doi.org/10.1007/s11336-016-9552-7",
      "10.1007/s11336-016-9552-7",
      "The asymptotically unbiased estimate of the population SRMR, which removes on the squared scale the bias by which the sample SRMR overestimates it, with its standard error.",
      "A fitted model whose unbiased SRMR is reported beside the SRMR (one group of continuous items fit by maximum likelihood to complete data).",
      "DMAR,lavaan"
    ),
    row(
      "usrmr_interval", "method",
      "Maydeu-Olivares, A., Shi, D., & Rosseel, Y. (2018). Assessing fit in structural equation models: A Monte-Carlo evaluation of RMSEA versus SRMR confidence intervals and tests of close fit. Structural Equation Modeling, 25(3), 389--402. https://doi.org/10.1080/10705511.2017.1389611",
      "10.1080/10705511.2017.1389611",
      "The large sample normal reference confidence interval of the population SRMR, which was accurate in models of 10 to 60 observed variables where the RMSEA interval was accurate only in small models.",
      "A fitted model whose unbiased SRMR is reported with its interval.",
      "DMAR,lavaan"
    ),
    # ---- reliability ----------------------------------------------------------
    row(
      "omega", "method",
      "McDonald, R. P. (1999). Test theory: A unified treatment. Lawrence Erlbaum. https://doi.org/10.4324/9781410601087",
      "10.4324/9781410601087",
      "Coefficient omega: the reliability of a unit-weighted composite under the one-factor model, the squared sum of the loadings over the model-implied total variance.",
      "Omega with the model-implied denominator, reported beside robust omega and as the target of the congeneric planner.",
      "DMAR, lavaan"
    ),
    row(
      "omega_robust", "method",
      "Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for population reliability coefficients: Evaluation of methods, recommendations, and software for composite measures. Psychological Methods, 21(1), 69--92. https://doi.org/10.1037/a0040086",
      "10.1037/a0040086",
      "Hierarchical omega with the observed composite variance as its denominator (robust omega), its confidence intervals, among them the percentile bootstrap (DMAR's named interval for robust omega, which has no closed form) and the bootstrap standard error on the logit scale (their Eq. 32).",
      "Robust omega (observed denominator), SIEVER's default reliability coefficient for continuous items.",
      "DMAR, lavaan"
    ),
    row(
      "omega_categorical", "method",
      "Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for population reliability coefficients: Evaluation of methods, recommendations, and software for composite measures. Psychological Methods, 21(1), 69--92. https://doi.org/10.1037/a0040086",
      "10.1037/a0040086",
      "Categorical omega for ordered or binary items and its confidence intervals, among them the bias corrected and accelerated bootstrap (DMAR's default for it).",
      "Robust omega for ordered items (observed denominator built from the unrestricted polychoric correlations).",
      "DMAR, lavaan"
    ),
    row(
      "omega_numerator", "method",
      "Flora, D. B. (2020). Your coefficient alpha is probably wrong, but which coefficient omega is right? A tutorial on using R to obtain better reliability estimates. Advances in Methods and Practices in Psychological Science, 3(4), 484--501. https://doi.org/10.1177/2515245920951747",
      "10.1177/2515245920951747",
      "Which omega answers which question: the numerator comes from the factor model, so omega must be reported with the fit of that model, and a misspecified model is not repaired by the choice of denominator.",
      "Every reported omega.",
      "DMAR, lavaan"
    ),
    row(
      "omega_categorical_model_implied", "method",
      "Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using structural equation modeling: An alternative to coefficient alpha. Psychometrika, 74(1), 155--167. https://doi.org/10.1007/s11336-008-9099-3",
      "10.1007/s11336-008-9099-3",
      "Omega for ordered items with the total variance taken from the polychoric correlations the one-factor model implies (the model-implied denominator).",
      "Omega for ordered items (model-implied denominator) only; the observed denominator is cited to Kelley and Pornprasertmanit (2016).",
      "DMAR, lavaan"
    ),
    row(
      "coefficient_h", "method",
      "Hancock, G. R., & Mueller, R. O. (2001). Rethinking construct reliability within latent variable systems. In R. Cudeck, S. du Toit, & D. S\u00f6rbom (Eds.), Structural equation modeling: Present and future (pp. 195--216). Scientific Software International.",
      NA_character_,
      "Coefficient H, the construct replicability (maximal reliability) of a factor from its standardized loadings.",
      "H beside omega on the Reliability page. A book chapter with no DOI: verified against the edited volume (Cudeck, du Toit, & S\u00f6rbom, 2001, Scientific Software International), not CrossRef.",
      "DMAR, lavaan"
    ),
    row(
      "alpha", "method",
      "Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. Psychometrika, 16(3), 297--334. https://doi.org/10.1007/BF02310555",
      "10.1007/BF02310555",
      "Coefficient alpha, the legacy coefficient, which equals the reliability of the composite only under essential tau equivalence and is otherwise a lower bound.",
      "Alpha as the flagged legacy alternative beside omega, never as the primary coefficient.",
      "DMAR"
    ),
    # ---- resampling intervals -----------------------------------------------
    row(
      "bootstrap_percentile", "method",
      "Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. Chapman and Hall. https://doi.org/10.1007/978-1-4899-4541-9",
      "10.1007/978-1-4899-4541-9",
      "The bootstrap and its percentile confidence interval: the quantiles of the statistic over resamples of the observed rows.",
      "A percentile bootstrap interval (or a bootstrap standard error interval) that was run to completion (robust omega's default interval, run on request).",
      "DMAR, lavaan"
    ),
    row(
      "bootstrap_guidance", "method",
      "Hesterberg, T. C. (2015). What teachers should know about the bootstrap: Resampling in the undergraduate statistics curriculum. The American Statistician, 69(4), 371--386. https://doi.org/10.1080/00031305.2015.1089789",
      "10.1080/00031305.2015.1089789",
      "The number of replications a bootstrap interval needs for its Monte Carlo error to be small, and the small-sample behavior of the percentile interval.",
      "Every completed bootstrap interval, with the replications used stated beside it.",
      "DMAR, lavaan"
    ),
    row(
      "bootstrap_bca", "method",
      "Efron, B. (1987). Better bootstrap confidence intervals. Journal of the American Statistical Association, 82(397), 171--185. https://doi.org/10.1080/01621459.1987.10478410",
      "10.1080/01621459.1987.10478410",
      "The bias corrected and accelerated (BCa) bootstrap interval.",
      "A BCa bootstrap interval that was run to completion (categorical omega's default interval, DMAR's).",
      "DMAR, boot"
    ),
    # ---- validity -------------------------------------------------------------
    row(
      "htmt", "method",
      "Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for assessing discriminant validity in variance-based structural equation modeling. Journal of the Academy of Marketing Science, 43(1), 115--135. https://doi.org/10.1007/s11747-014-0403-8",
      "10.1007/s11747-014-0403-8",
      "The heterotrait-monotrait ratio of correlations (HTMT) as a criterion of discriminant validity.",
      "HTMT on the Validity page.",
      "DMAR"
    ),
    row(
      "ave", "method",
      "Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models with unobservable variables and measurement error. Journal of Marketing Research, 18(1), 39--50. https://doi.org/10.1177/002224378101800104",
      "10.1177/002224378101800104",
      "Average variance extracted and its comparison with the squared factor correlations as a criterion of discriminant validity.",
      "AVE on the Validity page.",
      "DMAR, lavaan"
    ),
    row(
      "marker", "method",
      "Lindell, M. K., & Whitney, D. J. (2001). Accounting for common method variance in cross-sectional research designs. Journal of Applied Psychology, 86(1), 114--121. https://doi.org/10.1037/0021-9010.86.1.114",
      "10.1037/0021-9010.86.1.114",
      "The marker variable adjustment: a theoretically unrelated marker's smallest correlation estimates the common method variance partialled from the observed correlations.",
      "The marker adjustment of criterion correlations on the Validity page.",
      "SIEVER"
    ),
    row(
      "cmv", "method",
      "Podsakoff, P. M., MacKenzie, S. B., Lee, J.-Y., & Podsakoff, N. P. (2003). Common method biases in behavioral research: A critical review of the literature and recommended remedies. Journal of Applied Psychology, 88(5), 879--903. https://doi.org/10.1037/0021-9010.88.5.879",
      "10.1037/0021-9010.88.5.879",
      "Common method variance, its sources, and the statistical remedies, among them the unmeasured latent method factor.",
      "The unmeasured latent method factor model and the Harman screen on the Validity page.",
      "lavaan"
    ),
    # ---- dimensionality -------------------------------------------------------
    row(
      "parallel_analysis", "method",
      "Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. Psychometrika, 30(2), 179--185. https://doi.org/10.1007/BF02289447",
      "10.1007/BF02289447",
      "Parallel analysis: eigenvalues of the observed correlations compared with those of random data of the same size.",
      "Parallel analysis on the Dimensionality page.",
      "DMAR"
    ),
    row(
      "parallel_analysis_threshold", "method",
      "Glorfeld, L. W. (1995). An improvement on Horn's parallel analysis methodology for selecting the correct number of factors to retain. Educational and Psychological Measurement, 55(3), 377--393. https://doi.org/10.1177/0013164495055003002",
      "10.1177/0013164495055003002",
      "Comparing the observed eigenvalues with an upper quantile of the random eigenvalues rather than their mean reduces overextraction.",
      "Parallel analysis with a quantile criterion.",
      "DMAR"
    ),
    row(
      "parallel_analysis_ordered", "method",
      "Timmerman, M. E., & Lorenzo-Seva, U. (2011). Dimensionality assessment of ordered polytomous items with parallel analysis. Psychological Methods, 16(2), 209--220. https://doi.org/10.1037/a0023353",
      "10.1037/a0023353",
      "Parallel analysis of ordered items on polychoric correlations with minimum rank factor analysis.",
      "Parallel analysis on polychoric correlations (ordered or binary items).",
      "DMAR"
    ),
    row(
      "map", "method",
      "Velicer, W. F. (1976). Determining the number of components from the matrix of partial correlations. Psychometrika, 41(3), 321--327. https://doi.org/10.1007/BF02293557",
      "10.1007/BF02293557",
      "The minimum average partial (MAP) criterion for the number of dimensions.",
      "A recorded MAP analysis in a saved study. SIEVER runs no MAP analysis of its own; the EFA page's factor count comparison (DMAR::factor_count()) reports Velicer's MAP beside the factor models.",
      "psych"
    ),
    row(
      "vss", "method",
      "Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative procedure for estimating the optimal number of interpretable factors. Multivariate Behavioral Research, 14(4), 403--414. https://doi.org/10.1207/s15327906mbr1404_2",
      "10.1207/s15327906mbr1404_2",
      "The very simple structure (VSS) criterion for the number of interpretable factors.",
      "VSS on the Dimensionality page, computed by SIEVER from DMAR::fa_exploratory() solutions.",
      "DMAR"
    ),
    # ---- invariance and alignment ---------------------------------------------
    row(
      "invariance", "method",
      "Meredith, W. (1993). Measurement invariance, factor analysis and factorial invariance. Psychometrika, 58(4), 525--543. https://doi.org/10.1007/BF02294825",
      "10.1007/BF02294825",
      "Measurement invariance as a sequence of nested factor models: configural, weak (loadings), strong (intercepts) and strict (residual variances).",
      "The invariance ladder on the CV and Invariance page and the longitudinal rungs.",
      "DMAR, lavaan"
    ),
    row(
      "invariance_fit", "method",
      "Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit indexes for testing measurement invariance. Structural Equation Modeling, 9(2), 233--255. https://doi.org/10.1207/S15328007SEM0902_5",
      "10.1207/S15328007SEM0902_5",
      "A change in CFI of .01 or less between adjacent rungs as the practical criterion of invariance.",
      "Every judged invariance rung.",
      "DMAR, lavaan"
    ),
    row(
      "invariance_fit_rmsea", "method",
      "Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of measurement invariance. Structural Equation Modeling, 14(3), 464--504. https://doi.org/10.1080/10705510701301834",
      "10.1080/10705510701301834",
      "Changes in RMSEA and SRMR, beside the CFI, as criteria of invariance, with the sample size and the pattern of noninvariance taken into account.",
      "Every judged invariance rung.",
      "DMAR, lavaan"
    ),
    row(
      "invariance_ordered", "method",
      "Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor analysis models of different levels of invariance for ordered categorical outcomes. Psychometrika, 81(4), 1014--1045. https://doi.org/10.1007/s11336-016-9506-0",
      "10.1007/s11336-016-9506-0",
      "Identification of the invariance rungs for ordered items: thresholds and loadings, and the parameterization each rung needs.",
      "An invariance ladder fit on ordered or binary items.",
      "DMAR, lavaan"
    ),
    row(
      "partial_invariance", "method",
      "Byrne, B. M., Shavelson, R. J., & Muth\u00e9n, B. (1989). Testing for the equivalence of factor covariance and mean structures: The issue of partial measurement invariance. Psychological Bulletin, 105(3), 456--466. https://doi.org/10.1037/0033-2909.105.3.456",
      "10.1037/0033-2909.105.3.456",
      "Partial measurement invariance: freeing the parameters of some items across groups while the anchors stay constrained.",
      "Partial invariance and the anchor sensitivity comparison.",
      "DMAR, lavaan"
    ),
    row(
      "alignment", "method",
      "Asparouhov, T., & Muth\u00e9n, B. (2014). Multiple-group factor analysis alignment. Structural Equation Modeling, 21(4), 495--508. https://doi.org/10.1080/10705511.2014.919210",
      "10.1080/10705511.2014.919210",
      "Alignment: approximate invariance that minimizes the total noninvariance of loadings and intercepts across groups without exact equality constraints.",
      "Alignment on the CV and Invariance page.",
      "DMAR"
    ),
    # ---- item response theory ------------------------------------------------
    row(
      "irt_grm", "method",
      "Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. Psychometrika, 34(S1), 1--97. https://doi.org/10.1007/BF03372160",
      "10.1007/BF03372160",
      "The graded response model for ordered polytomous items.",
      "The graded response model on the IRT page and its evidence (parameter intervals, information, residual pairs).",
      "DMAR, lavaan"
    ),
    # ---- cross-validation, scores, composites, bifactor, mediation ----------
    row(
      "cross_validation", "method",
      "Browne, M. W. (2000). Cross-validation methods. Journal of Mathematical Psychology, 44(1), 108--132. https://doi.org/10.1006/jmps.1999.1279",
      "10.1006/jmps.1999.1279",
      "Cross-validation of a covariance structure model: the fit of a model estimated in a calibration sample to a validation sample.",
      "The calibration and validation split on the CV and Invariance page.",
      "lavaan"
    ),
    row(
      "factor_scores", "method",
      "Estabrook, R., & Neale, M. (2013). A comparison of factor score estimation methods in the presence of missing data: Reliability and an application to nicotine dependence. Multivariate Behavioral Research, 48(1), 1--27. https://doi.org/10.1080/00273171.2012.730072",
      "10.1080/00273171.2012.730072",
      "Factor score estimation under incomplete data, with full information estimates compared with scores from imputed or listwise data.",
      "Factor scores and their uncertainty on the Reliability page.",
      "DMAR,lavaan"
    ),
    row(
      "composite_scores", "method",
      "Bollen, K., & Lennox, R. (1991). Conventional wisdom on measurement: A structural equation perspective. Psychological Bulletin, 110(2), 305--314. https://doi.org/10.1037/0033-2909.110.2.305",
      "10.1037/0033-2909.110.2.305",
      "Effect indicators and causal indicators, and what a composite of effect indicators measures.",
      "Operational scores and the composite reliability of a weighted composite.",
      "SIEVER"
    ),
    row(
      "composite_indicators", "method",
      "Bollen, K. A., & Bauldry, S. (2011). Three Cs in measurement models: Causal indicators, composite indicators, and covariates. Psychological Methods, 16(3), 265--284. https://doi.org/10.1037/a0024448",
      "10.1037/a0024448",
      "The distinction between causal indicators, composite indicators and covariates in a measurement model.",
      "A composite built from a fitted model.",
      "SIEVER, lavaan"
    ),
    row(
      "bifactor", "method",
      "Reise, S. P. (2012). The rediscovery of bifactor measurement models. Multivariate Behavioral Research, 47(5), 667--696. https://doi.org/10.1080/00273171.2012.715555",
      "10.1080/00273171.2012.715555",
      "The bifactor model: one general factor and orthogonal group factors, and what its structure says about a total score.",
      "The bifactor model on the Bifactor page.",
      "lavaan"
    ),
    row(
      "bifactor_indices", "method",
      "Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating bifactor models: Calculating and interpreting statistical indices. Psychological Methods, 21(2), 137--150. https://doi.org/10.1037/met0000045",
      "10.1037/met0000045",
      "Omega hierarchical, explained common variance, the percentage of uncorrelated correlations and H for a bifactor solution.",
      "The bifactor indices and their bootstrap on the Bifactor page.",
      "DMAR, lavaan"
    ),
    row(
      "indirect_mc", "method",
      "Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo confidence intervals for indirect effects. Communication Methods and Measures, 6(2), 77--98. https://doi.org/10.1080/19312458.2012.679848",
      "10.1080/19312458.2012.679848",
      "The Monte Carlo confidence interval for an indirect effect, from draws of the parameter estimates under their joint sampling distribution.",
      "Monte Carlo intervals of defined effects on the SEM page.",
      "lavaan"
    ),
    row(
      "aipe", "method",
      "Kelley, K., & Maxwell, S. E. (2003). Sample size for multiple regression: Obtaining regression coefficients that are accurate, not simply significant. Psychological Methods, 8(3), 305--321. https://doi.org/10.1037/1082-989X.8.3.305",
      "10.1037/1082-989X.8.3.305",
      "Accuracy in parameter estimation: planning the sample size so that the confidence interval is sufficiently narrow, as distinct from planning for power.",
      "The congeneric reliability planner.",
      "DMAR"
    ),
    # ---- reporting and scale development ---------------------------------
    row(
      "reporting", "method",
      "Flake, J. K., & Fried, E. I. (2020). Measurement schmeasurement: Questionable measurement practices and how to avoid them. Advances in Methods and Practices in Psychological Science, 3(4), 456--465. https://doi.org/10.1177/2515245920952393",
      "10.1177/2515245920952393",
      "The measurement decisions a report must disclose: what was measured, how the scores were formed, and which analyses were run with which settings.",
      "The methods draft and the report from recorded analyses.",
      "SIEVER"
    ),
    row(
      "scale_development", "method",
      "Clark, L. A., & Watson, D. (2019). Constructing validity: New developments in creating objective measuring instruments. Psychological Assessment, 31(12), 1412--1427. https://doi.org/10.1037/pas0000626",
      "10.1037/pas0000626",
      "The steps of scale development, from the construct definition and the item pool to structural validity and external validity.",
      "Not attached to an analysis kind; available to the researcher for the development narrative.",
      "SIEVER"
    ),
    # ---- psychometric network models ------------------------------------------
    row(
      "network_ggm", "method",
      "Epskamp, S., Rhemtulla, M., & Borsboom, D. (2017). Generalized network psychometrics: Combining network and latent variable models. Psychometrika, 82(4), 904--927. https://doi.org/10.1007/s11336-017-9557-x",
      "10.1007/s11336-017-9557-x",
      "The Gaussian graphical model as a psychometric model of the items, and its two combinations with a latent variable model: the residual network model (a measurement model with a partial correlation network among the residuals) and the latent network model (a partial correlation network among the latent variables).",
      "Every network model on the Network page.",
      "psychonetrics"
    ),
    row(
      "network_partial_correlation", "method",
      "Epskamp, S., & Fried, E. I. (2018). A tutorial on regularized partial correlation networks. Psychological Methods, 23(4), 617--634. https://doi.org/10.1037/met0000167",
      "10.1037/met0000167",
      "What a partial correlation network is and how its edges are read: the correlation between two variables given every other variable in the set, with a zero edge as conditional independence.",
      "Every network model on the Network page.",
      "psychonetrics"
    ),
    row(
      "network_accuracy", "method",
      "Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating psychological networks and their accuracy: A tutorial paper. Behavior Research Methods, 50(1), 195--212. https://doi.org/10.3758/s13428-017-0862-1",
      "10.3758/s13428-017-0862-1",
      "Edge weights and centrality indices are estimates with sampling error and are reported with their uncertainty; the bootstrap as the general method for the accuracy of a network.",
      "The standard errors and intervals of the edges, and the reading of strength and expected influence, on the Network page.",
      "psychonetrics"
    ),
    row(
      "network_model_search", "method",
      "Epskamp, S. (2020). Psychometric network models from time-series and panel data. Psychometrika, 85(1), 206--231. https://doi.org/10.1007/s11336-020-09697-3",
      "10.1007/s11336-020-09697-3",
      "The psychonetrics framework and its model search: estimate the saturated network, prune the edges not significant at alpha, and step up by modification indices while the criterion improves.",
      "The sparse solution of every network on the Network page (prune at alpha, then step up).",
      "psychonetrics"
    ),
    row(
      "network_factor_equivalence", "method",
      "van Bork, R., Rhemtulla, M., Waldorp, L. J., Kruis, J., Rezvanifar, S., & Borsboom, D. (2021). Latent variable models and networks: Statistical equivalence and testability. Multivariate Behavioral Research, 56(2), 175--198. https://doi.org/10.1080/00273171.2019.1672515",
      "10.1080/00273171.2019.1672515",
      "A unidimensional factor model and a fully connected Gaussian graphical model imply the same covariance matrix, so a network and a factor model are not told apart by fit; what is testable is the sparsity.",
      "The latent network model, and the reading of the item network beside the factor models.",
      "psychonetrics"
    ),
    row(
      "network_ising_irt", "method",
      "Marsman, M., Borsboom, D., Kruis, J., Epskamp, S., van Bork, R., Waldorp, L. J., van der Maas, H. L. J., & Maris, G. (2018). An introduction to network psychometrics: Relating Ising network models to item response theory models. Multivariate Behavioral Research, 53(1), 15--35. https://doi.org/10.1080/00273171.2017.1379379",
      "10.1080/00273171.2017.1379379",
      "For binary items the Ising network model and a multidimensional item response model are statistically equivalent, the categorical counterpart of the network and factor equivalence.",
      "A network of ordered or binary items (DWLS on polychoric correlations) on the Network page.",
      "psychonetrics"
    ),
    row(
      "network_strength", "method",
      "Opsahl, T., Agneessens, F., & Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social Networks, 32(3), 245--251. https://doi.org/10.1016/j.socnet.2010.03.006",
      "10.1016/j.socnet.2010.03.006",
      "Node strength, the sum of the weights of a node's edges, as the weighted generalization of degree.",
      "Strength on the Network page.",
      "SIEVER"
    ),
    row(
      "network_expected_influence", "method",
      "Robinaugh, D. J., Millner, A. J., & McNally, R. J. (2016). Identifying highly influential nodes in the complicated grief network. Journal of Abnormal Psychology, 125(6), 747--757. https://doi.org/10.1037/abn0000181",
      "10.1037/abn0000181",
      "Expected influence, the signed sum of a node's edge weights, as the centrality that respects negative edges.",
      "Expected influence on the Network page.",
      "SIEVER"
    ),
    row(
      "network_communities", "method",
      "Pons, P., & Latapy, M. (2006). Computing communities in large networks using random walks. Journal of Graph Algorithms and Applications, 10(2), 191--218. https://doi.org/10.7155/jgaa.00124",
      "10.7155/jgaa.00124",
      "The walktrap algorithm: the communities of a weighted graph from short random walks.",
      "The communities of the item and residual networks on the Network page.",
      "igraph"
    ),
    row(
      "network_ega", "method",
      "Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A new approach for estimating the number of dimensions in psychological research. PLOS ONE, 12(6), e0174035. https://doi.org/10.1371/journal.pone.0174035",
      "10.1371/journal.pone.0174035",
      "Exploratory graph analysis: walktrap communities of a psychometric network read as the dimensions of an item pool.",
      "The reading of the communities against the ledger's constructs on the Network page.",
      "igraph"
    ),
    # ---- software -------------------------------------------------------------
    row(
      "R", "software",
      "R Core Team (YEAR). R: A language and environment for statistical computing (Version VERSION) [Computer software]. R Foundation for Statistical Computing. https://www.R-project.org/",
      "https://www.R-project.org/",
      "The computing environment every analysis ran in.",
      "Always cited, with the running version."
    ),
    row(
      "SIEVER", "software",
      "Kelley, K. (2026). SIEVER: Scale item evaluation and validation environment in R (Version VERSION) [R package].",
      NA_character_,
      "The environment that recorded the analyses, their specifications and their revisions (the entry citation(\"SIEVER\") gives).",
      "Always cited, with the version that made the record."
    ),
    row(
      "DMAR", "software",
      "Kelley, K. (YEAR). DMAR: Design, measurement, and analysis in R (Version VERSION) [R package]. https://doi.org/10.5281/zenodo.22960335",
      "10.5281/zenodo.22960335",
      "Reliability coefficients and their intervals, measurement invariance, alignment, the graded response model, the bifactor indices, and every exploratory factor analysis (the EFA page, the Items page's live model, the solutions behind VSS and the Optimizer's simple structure) and parallel analysis.",
      "Cited when a method that runs through DMAR was used."
    ),
    row(
      "lavaan", "software",
      "Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. Journal of Statistical Software, 48(2), 1--36. https://doi.org/10.18637/jss.v048.i02 (lavaan version VERSION)",
      "10.18637/jss.v048.i02",
      "The structural equation modeling engine behind every confirmatory fit.",
      "Cited when a method that runs through lavaan was used."
    ),
    row(
      "psych", "software",
      "Revelle, W. (YEAR). psych: Procedures for psychological, psychometric, and personality research (Version VERSION) [R package]. Northwestern University. https://CRAN.R-project.org/package=psych",
      "https://CRAN.R-project.org/package=psych",
      "The polychoric, tetrachoric and mixed correlation matrices, and the Screening page's skewness and kurtosis.",
      "Cited when a method that runs through psych was used."
    ),
    row(
      "boot", "software",
      "Canty, A., Ripley, B. D., & Brazzale, A. R. (YEAR). boot: Bootstrap functions (Version VERSION) [R package]. https://CRAN.R-project.org/package=boot",
      "https://CRAN.R-project.org/package=boot",
      "The BCa bootstrap interval the engine forms.",
      "Cited when DMAR's BCa bootstrap was used."
    ),
    row(
      "psychonetrics", "software",
      "Epskamp, S. (YEAR). psychonetrics: Structural equation modeling and confirmatory network analysis (Version VERSION) [R package]. https://doi.org/10.32614/CRAN.package.psychonetrics",
      "10.32614/CRAN.package.psychonetrics",
      "The engine of every network model: the Gaussian graphical, residual network and latent network models, their search, standard errors and fit measures.",
      "Cited when a network model was estimated."
    ),
    row(
      "qgraph", "software",
      "Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. Journal of Statistical Software, 48(4), 1--18. https://doi.org/10.18637/jss.v048.i04 (qgraph version VERSION)",
      "10.18637/jss.v048.i04",
      "The network figures (spring layout, edge color by sign and width by size).",
      "Cited when a network figure was drawn."
    ),
    row(
      "igraph", "software",
      "Cs\u00e1rdi, G., & Nepusz, T. (2006). The igraph software package for complex network research. InterJournal, Complex Systems, 1695. https://igraph.org (igraph version VERSION)",
      "https://igraph.org",
      "The walktrap communities of a network.",
      "Cited when communities were computed."
    )
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The Methods a Recorded Specification Used, as Registry Keys
#'
#' Reads the kind and the settings of an analysis specification (see
#' `sieve_register_analysis()`) and returns the keys of
#' [sieve_method_references()] for the methods it ran: the estimator family
#' (MLR, ML, WLSMV), FIML, the fit indices of a fitted model (with the
#' unbiased SRMR and its interval when the record holds them), the reliability
#' coefficients of each group with the interval method actually used (a
#' bootstrap only when it ran to completion), H and alpha when present, the
#' invariance rungs, alignment, the graded response model, the
#' cross-validation split, the validity criteria, and the dimensionality
#' criteria. Keys a caller names in `spec$methods` or `extra` are added.
#' A method is listed only when the record shows it was run, so a pending
#' bootstrap and a failed fit contribute nothing.
#'
#' @param spec The specification of a recorded analysis.
#' @param estimates The record's stored results, or `NULL`.
#' @param fitted Whether the record holds a solution (fit indices are cited
#'   only then).
#' @param extra Further keys the caller supplies.
#' @return A character vector of unique keys, possibly empty.
#' @author Ken Kelley
#' @noRd
sieve_spec_methods <- function(spec, estimates = NULL, fitted = !is.null(estimates), extra = NULL) {
  keys <- unique(as.character(c(extra, if (is.list(spec)) spec$methods)))
  if (!is.list(spec)) {
    return(keys)
  }
  kind <- as.character(spec$kind %||% "")
  if (!length(kind) || is.na(kind)) kind <- ""
  estimator_keys <- function(estimator, missing = NULL, ordered = NULL) {
    est <- toupper(as.character(estimator %||% ""))
    c(
      if (identical(est, "MLR")) "mlr",
      if (identical(est, "ML")) "ml",
      if (est %in% c("WLSMV", "DWLS", "ULSMV", "WLS")) c("wlsmv", "categorical_vs_continuous"),
      if (identical(as.character(missing %||% ""), "fiml")) "fiml"
    )
  }
  model_kinds <- c(
    "cfa", "sem", "bifactor", "method_factor", "longitudinal", "invariance",
    "reliability_H", "irt", "cross_validation", "anchor_sensitivity", "alignment"
  )
  if (kind %in% model_kinds) {
    keys <- c(keys, estimator_keys(spec$estimator, spec$missing, spec$ordered))
    if (isTRUE(fitted) && kind %in% c("cfa", "sem", "bifactor", "method_factor", "longitudinal", "invariance", "irt")) {
      keys <- c(keys, "fit_indices", "fit_indices_caution")
      fit <- if (is.list(estimates)) estimates$fit %||% estimates$measures else NULL
      df <- if (is.numeric(fit) && "df" %in% names(fit)) unname(fit["df"]) else NA_real_
      if (is.finite(df) && df < 10) keys <- c(keys, "rmsea_small_df")
      # The unbiased SRMR and its interval, when the record holds a value
      # (DMAR computed it; see sieve_fit_usrmr()).
      usrmr <- if (is.numeric(fit)) attr(fit, "families")$usrmr else NULL
      if (isTRUE(usrmr$available)) {
        keys <- c(keys, "usrmr", if (is.finite(usrmr$lower) && is.finite(usrmr$upper)) "usrmr_interval")
      }
    }
    keys <- c(keys, switch(kind,
      cfa = "cfa",
      sem = "cfa",
      bifactor = c("cfa", "bifactor"),
      method_factor = c("cfa", "cmv"),
      longitudinal = ,
      invariance = c("cfa", "invariance", "invariance_fit", "invariance_fit_rmsea",
        if (length(spec$ordered %||% character(0))) "invariance_ordered"),
      anchor_sensitivity = c("cfa", "invariance", "partial_invariance"),
      alignment = "alignment",
      irt = "irt_grm",
      cross_validation = c("cfa", "cross_validation"),
      reliability_H = "coefficient_h",
      character(0)
    ))
  }
  if (identical(kind, "reliability")) {
    groups <- spec$groups %||% list()
    for (i in seq_along(groups)) {
      g <- groups[[i]]
      keys <- c(keys, switch(as.character(g$coefficient %||% ""),
        omega_robust = c("omega_robust", "omega_numerator"),
        omega = c("omega", "omega_numerator"),
        omega_categorical = c(
          if (identical(g$denominator, "model_implied")) "omega_categorical_model_implied" else "omega_categorical",
          "omega_numerator"
        ),
        character(0)
      ))
      if (identical(g$missing, "fiml")) keys <- c(keys, "fiml")
      used <- as.character(g$ci_method %||% "none")
      keys <- c(keys, switch(used,
        percentile = ,
        dmar_percentile = c("bootstrap_percentile", "bootstrap_guidance"),
        dmar_bca = c("bootstrap_bca", "bootstrap_guidance"),
        bootstrap_se_logistic = c("bootstrap_percentile", "bootstrap_guidance"),
        mlr = , mlr_logistic = "mlr",
        ml = , ml_logistic = "ml",
        character(0)
      ))
      e <- if (is.list(estimates) && length(estimates) >= i) estimates[[i]] else NULL
      if (is.list(e)) {
        if (is.list(e$H) && is.finite(e$H$H %||% NA_real_)) keys <- c(keys, "coefficient_h")
        if (is.list(e$alpha) && is.finite(e$alpha$estimate %||% NA_real_)) keys <- c(keys, "alpha")
        mi <- e$model_implied
        if (is.list(mi) && is.finite(mi$estimate %||% NA_real_)) {
          keys <- c(keys, "omega", switch(as.character(mi$uncertainty$method %||% ""), mlr = , mlr_logistic = "mlr",
            ml = , ml_logistic = "ml", character(0)))
        }
      }
    }
  }
  if (startsWith(kind, "IRT")) {
    keys <- c(keys, "irt_grm", estimator_keys(spec$estimator))
  }
  # The network models of the Network page (fct_network.R): the framework,
  # the reading of a partial correlation network, the search, the accuracy
  # of the edges; the estimator family; the fit indices when the sparse
  # model has a solution; the measurement model behind the residual and
  # latent networks; the node summaries and communities of the item and
  # residual networks; the equivalence results the page teaches.
  if (startsWith(kind, "network_")) {
    engine <- toupper(as.character(spec$estimator %||% ""))
    keys <- c(
      keys, "network_ggm", "network_partial_correlation", "network_model_search", "network_accuracy",
      switch(engine,
        DWLS = , WLS = , ULS = c("wlsmv", "categorical_vs_continuous", "network_ising_irt"),
        MLR = "mlr", FIML = "fiml", ML = "ml", character(0)
      ),
      if (isTRUE(fitted)) c("fit_indices", "fit_indices_caution"),
      switch(kind,
        network_ggm = c("network_strength", "network_expected_influence", "network_communities", "network_ega"),
        network_rnm = c("cfa", "network_strength", "network_expected_influence", "network_communities", "network_ega"),
        network_lnm = c("cfa", "network_factor_equivalence"),
        character(0)
      ),
      "psychonetrics", "qgraph", "igraph"
    )
  }
  if (kind %in% c("model_composite", "latent_boundary", "factor_score_uncertainty", "indirect_mc", "longitudinal_comparison")) {
    parent <- spec$parent_spec
    if (is.list(parent)) keys <- c(keys, estimator_keys(parent$estimator, parent$missing))
    keys <- c(keys, switch(kind,
      model_composite = c("composite_scores", "composite_indicators"),
      factor_score_uncertainty = "factor_scores",
      indirect_mc = "indirect_mc",
      longitudinal_comparison = c("invariance", "invariance_fit", "invariance_fit_rmsea"),
      character(0)
    ))
  }
  keys <- c(keys, switch(kind,
    categorical_omega_mi = c(if (identical(spec$denominator, "model_implied")) "omega_categorical_model_implied" else "omega_categorical",
      "multiple_imputation", "imputation_count", "mice", "bootstrap_mi", "bootstrap_mi_compatibility"),
    ordered_mi = c("cfa", "wlsmv", "multiple_imputation", "imputation_count", "mice", "lavaan.mi"),
    score_policy_sensitivity = "composite_scores",
    congeneric_aipe = c("aipe", "omega"),
    htmt = "htmt",
    ave = "ave",
    marker = "marker",
    harman = "cmv",
    parallel_analysis = c("parallel_analysis", "parallel_analysis_threshold",
      if (as.character(spec$basis %||% spec$cor %||% "") %in% c("polychoric", "tetrachoric", "poly", "tet", "mixed")) "parallel_analysis_ordered"),
    map = "map",
    vss = "vss",
    reliability_bootstrap = switch(as.character(spec$ci_method %||% ""),
      percentile = , dmar_percentile = , bootstrap_se_logistic = c("bootstrap_percentile", "bootstrap_guidance"),
      dmar_bca = c("bootstrap_bca", "bootstrap_guidance"), character(0)),
    bifactor_bootstrap = c("bifactor", "bifactor_indices", "bootstrap_percentile", "bootstrap_guidance"),
    character(0)
  ))
  unique(keys[!is.na(keys) & nzchar(keys)])
}

#' The version of a package to cite
#'
#' @param pkg A package name, `"R"`, or `"SIEVER"`.
#' @param versions A named list of recorded versions (as the analysis
#'   registry keeps them), or `NULL`.
#' @return A version string: the recorded version(s) when given, otherwise
#'   the installed one, or `"not installed"`.
#' @author Ken Kelley
#' @noRd
sieve_software_version <- function(pkg, versions = NULL) {
  recorded <- unlist(versions[[pkg]], use.names = FALSE)
  recorded <- unique(as.character(recorded[!is.na(recorded) & nzchar(recorded)]))
  if (length(recorded)) {
    return(paste(recorded, collapse = " and "))
  }
  if (identical(pkg, "R")) {
    return(paste(R.version$major, R.version$minor, sep = "."))
  }
  if (identical(pkg, "SIEVER")) {
    return(sieve_version())
  }
  tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "not installed")
}

#' The year of a package's installed release
#'
#' @param pkg A package name or `"R"`.
#' @return A four character year: the package's `Date` or
#'   `Date/Publication` field, the running R's year for `"R"`, otherwise
#'   the current year.
#' @author Ken Kelley
#' @noRd
sieve_software_year <- function(pkg) {
  if (identical(pkg, "R")) {
    return(as.character(R.version$year))
  }
  desc <- tryCatch(utils::packageDescription(pkg), error = function(e) NULL)
  dated <- if (is.list(desc)) c(desc$Date, desc$`Date/Publication`) else NULL
  year <- regmatches(dated, regexpr("[0-9]{4}", dated))
  if (length(year)) year[1L] else format(Sys.Date(), "%Y")
}

#' A Bibliography of the Methods Used and the Software They Ran In
#'
#' Formats the deduplicated reference entries of the given registry keys
#' (see [sieve_method_references()]): the methods in alphabetical order,
#' then the software those methods run through in SIEVER, plus R and
#' SIEVER themselves, each with the version actually used. Unverified
#' registry entries and unknown keys are left out, so the list never
#' cites a method that was not run and never cites an entry whose
#' bibliographic record could not be verified.
#'
#' @param keys Registry keys, as [sieve_method_references()] names them,
#'   or as `sieve_register_analysis()` records them in a record's
#'   `methods` field. A software name among the keys is cited as software.
#' @param versions A named list of version strings to cite for the
#'   software (for example the `versions` field of the analysis records
#'   the keys came from); a package not in it is cited with its installed
#'   version.
#' @param software Whether to list the software (with R and SIEVER
#'   always among it).
#' @return A character vector of reference entries, methods first, with an
#'   attribute `omitted` naming the keys that are in the registry but not
#'   verified (so a caller can say what is not listed).
#' @author Ken Kelley
#' @examples
#' sieve_method_bibliography(c("cfa", "mlr", "fiml", "omega_robust"))
#' sieve_method_bibliography("parallel_analysis", software = FALSE)
#' @export
sieve_method_bibliography <- function(keys, versions = NULL, software = TRUE) {
  registry <- sieve_method_references()
  keys <- unique(as.character(keys))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  methods <- registry[registry$kind == "method" & registry$key %in% keys, , drop = FALSE]
  omitted <- methods$key[!methods$verified]
  cited <- enc2utf8(methods$citation[methods$verified])
  # A radix sort orders the same way in every locale (reproducible reports)
  # and needs text marked UTF-8, Latin-1 or bytes.
  out <- sort(unique(cited), method = "radix")
  if (isTRUE(software)) {
    packages <- unique(c(
      "SIEVER", "R",
      unlist(strsplit(methods$software[methods$verified & !is.na(methods$software)], ",\\s*")),
      intersect(keys, registry$key[registry$kind == "software"])
    ))
    rows <- registry[registry$kind == "software" & registry$key %in% packages & registry$verified, , drop = FALSE]
    rows <- rows[match(intersect(packages, rows$key), rows$key), , drop = FALSE]
    entries <- vapply(seq_len(nrow(rows)), function(i) {
      pkg <- rows$key[i]
      text <- rows$citation[i]
      text <- gsub("VERSION", sieve_software_version(pkg, versions), text, fixed = TRUE)
      gsub("YEAR", sieve_software_year(pkg), text, fixed = TRUE)
    }, character(1))
    out <- c(out, enc2utf8(entries))
  }
  attr(out, "omitted") <- unique(omitted)
  out
}

#' The methods and the software versions of a set of registry records
#'
#' @param records Analysis records (elements of `state$analyses`).
#' @return A list with `keys` (the union of the records' methods; a record
#'   without a methods field is read from its specification)
#'   and `versions` (a named list, one entry per package, of the versions
#'   the records recorded).
#' @author Ken Kelley
#' @noRd
sieve_records_methods <- function(records) {
  keys <- character(0)
  versions <- list()
  for (r in records) {
    if (!is.list(r)) next
    keys <- c(keys, as.character(r$methods %||% sieve_spec_methods(r$spec, r$estimates, fitted = !is.null(r$estimates))))
    v <- r$versions
    if (is.list(v)) {
      for (pkg in names(v)) {
        value <- as.character(v[[pkg]])
        if (length(value) == 1L && !is.na(value) && nzchar(value)) {
          versions[[pkg]] <- unique(c(versions[[pkg]], value))
        }
      }
    }
  }
  list(keys = unique(keys), versions = versions)
}

#' The reference section of a report, as lines
#'
#' @param keys Registry keys.
#' @param versions Recorded versions (see `sieve_records_methods()`).
#' @param markdown Whether to write the heading and the entries in
#'   markdown (the draft) or as plain lines (the text report).
#' @param heading The heading text.
#' @param intro The sentence under the heading, saying what the list is.
#' @return A character vector of lines.
#' @author Ken Kelley
#' @noRd
sieve_reference_lines <- function(keys, versions = NULL, markdown = TRUE, heading = "References",
                                  intro = paste(
                                    "The methods the analyses above used and the software they ran in, with the",
                                    "versions recorded; a method that was not run is not cited."
                                  )) {
  entries <- sieve_method_bibliography(keys, versions = versions)
  omitted <- attr(entries, "omitted")
  registry <- sieve_method_references()
  note <- if (length(omitted)) {
    sprintf(
      "Not listed, because no verified bibliographic record exists for it: %s.",
      paste(sprintf("%s (%s)", omitted, vapply(omitted, function(k) {
        # The authors and the year: the citation up to its first "(year)".
        text <- registry$citation[registry$key == k][1L]
        head <- regmatches(text, regexpr("^.*?\\([0-9]{4}[a-z]?\\)", text, perl = TRUE))
        if (length(head)) head else text
      }, character(1))), collapse = "; ")
    )
  }
  c(
    if (isTRUE(markdown)) paste0("## ", heading) else heading,
    "",
    "The methods the analyses above used and the software they ran in, with the versions used; a method that was not run is not cited.",
    "",
    if (isTRUE(markdown)) paste0("- ", entries) else entries,
    if (!is.null(note)) c("", note)
  )
}

# Workflow controls.

#' Effective diagram label size after fitting or fixed-width export
#' @param natural_width Natural diagram width in inches, at 10-point labels.
#' @param target_width Available width in the same units.
#' @param base_pt Natural label size in points.
#' @return Effective point size.
#' @author Ken Kelley
#' @noRd
sieve_diagram_effective_label <- function(natural_width, target_width, base_pt = 10) {
  if (length(natural_width) != 1L || length(target_width) != 1L || !is.finite(natural_width) || !is.finite(target_width) || natural_width <= 0 || target_width <= 0) return(NA_real_)
  base_pt * target_width / natural_width
}

#' Readable reviewed scientific plan
#' @param plan A prepared assistant plan.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_assistant_plan_ui <- function(plan) {
  if (is.null(plan)) return(shiny::p("Describe the goal and prepare a plan. Nothing runs automatically."))
  specs <- lapply(plan$specs, function(sp) {
    data.frame(
      scope = sp$construct %||% sp$scope %||% "current construct model",
      estimand = if (identical(
        plan$task,
        "reliability"
      )) "Declared complete-score reliability" else "Measurement model parameters and fit",
      estimator = sp$estimator %||% sp$engine %||% "see exact specification",
      missingness = sp$missing %||% sp$missing_rule %||% "listwise for this reliability service; see specification",
      items = length(sp$items %||% sp$observed %||% character(0)), stringsAsFactors = FALSE
    )
  })
  shiny::tagList(
    shiny::h6("Reviewed research plan"), shiny::p(shiny::strong("Question: "), plan$goal),
    shiny::tags$ul(lapply(plan$actions, shiny::tags$li)),
    if (length(specs)) sieve_plain_table_ui(do.call(rbind, specs)),
    shiny::p(class = "small", plan$cost),
    shiny::p(class = "small", paste("Input revision:", paste(unlist(plan$revision), collapse = "; "))),
    shiny::tags$details(
      shiny::tags$summary("Exact plan and specifications"),
      shiny::tags$pre(paste(
        utils::capture.output(dput(plan[c(
          "task",
          "goal",
          "specs",
          "revision",
          "key"
        )])),
        collapse = "\n"
      ))
    )
  )
}

#' Disclosure summary derived from the immutable reviewed payload
#' @param payload Prepared payload with its exact context and review key.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_payload_summary_ui <- function(payload) {
  if (is.null(payload)) return(shiny::p("No external payload prepared."))
  cx <- payload$context
  categories <- sieve_assistant_payload_counts(payload)
  narratives <- list(
    definitions = cx$constructs,
    claims = cx$claims,
    evaluations = cx$evaluations,
    decisions = cx$decisions
  )
  shiny::tagList(
    shiny::strong("Review this exact disclosure"),
    shiny::p(class = "small", paste("Provider:", payload$request$provider, "; model:", payload$request$model)),
    shiny::p(class = "small", paste("Review key:", payload$review_key)),
    sieve_plain_table_ui(categories),
    shiny::p(
      class = "small",
      paste(
        "Payload:",
        nchar(if (payload$request$provider == "OpenAI") payload$request$body$input else
          payload$request$body$messages[[1L]]$content, type = "chars"), "characters; approximately",
        ceiling(nchar(if (payload$request$provider == "OpenAI") payload$request$body$input else
          payload$request$body$messages[[1L]]$content, type = "chars") / 4),
        "input tokens (rough estimate); ceiling:", payload$request$max_chars,
        "characters. Charges depend on your selected model and provider; check its price before sending."
      )
    ),
    shiny::p("Narratives and analysis notes may quote private item wording even when dedicated wording columns are excluded. Inspect the full request before authorizing it."),
    shiny::tags$details(
      shiny::tags$summary("Included narratives"),
      shiny::tags$pre(paste(utils::capture.output(dput(narratives)),
        collapse = "\n"
      ))
    )
  )
}

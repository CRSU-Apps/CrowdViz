#' Get PopViz data from {metafor} object (rma.uni).
#'
#' @param model Metafor analysis object from which to get PopViz data
#'
#' @return List of plot options:
#' - "relative_effect": Point estimate of relative effect
#' - "confidence_interval": vector of lower and upper limits of confidence interval of relative effect
#' - "outcome_type": Type of outcome of analysis
#' @export
#'
#' @examples
#' ExtractMetafor(metafor_model)
ExtractMetafor <- function(model) {
  
  RelEff <- model$b
  RelConfInt <- c(model$ci.lb, model$ci.ub)
  OutcomeType <- model$measure
  
  return(
    list(
      relative_effect = RelEff,
      confidence_interval = RelConfInt,
      outcome_type = OutcomeType
    )
  )
}

#' Get PopViz data from {MetaStan} object (meta_stan)
#'
#' @param model MetaStan analysis object from which to get PopViz data
#'
#' @return List of plot options:
#' - "relative_effect": Point estimate of relative effect
#' - "confidence_interval": vector of lower and upper limits of confidence interval of relative effect
#' - "outcome_type": Type of outcome of analysis
#' @export
#'
#' @examples
#' ExtractMetaStan(metastan_model)
ExtractMetaStan <- function(model) {
  
  RelEff <- model$fit_sum['theta', c(1)]
  RelConfInt <- model$fit_sum['theta', c(4,8)]
  # To be confirmed once we understand MetaStan better
  return(
    list(
      relative_effect = RelEff,
      confidence_interval = RelConfInt,
      outcome_type = c(NA, NA)
    )
  )
}
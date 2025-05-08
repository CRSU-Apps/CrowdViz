## Functions for extracting relevant data from meta-analysis functions ##

# {metafor} object extraction (rma.uni)

#' Title
#'
#' @param model 
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

# {MetaStan} object extraction (meta_stan)

ExtractMetaStan <- function(model) {
  
  RelEff <- model$fit_sum['theta', c(1)]
  RelConfInt <- model$fit_sum['theta', c(4,8)]
  # To be confirmed once we understand MetaStan better
}
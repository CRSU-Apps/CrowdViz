## Functions for extracting relevant data from meta-analysis functions ##

# {metafor} object extraction (rma.uni)

ExtractMetafor <- function(model) {
  
  RelEff <- model$b
  RelConfInt <- c(model$ci.lb, model$ci.ub)
  OutcomeType <- model$measure
  
}

# {MetaStan} object extraction (meta_stan)

ExtractMetaStan <- function(model) {
  
  RelEff <- model$fit_sum['theta', c(1)]
  RelConfInt <- model$fit_sum['theta', c(4,8)]
  # To be confirmed once we understand MetaStan better
}
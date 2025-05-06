
# {metafor} object extraction (rma.uni)

MetaforExtract <- function(model) {

  RelEff <- model$b
  RelConfInt <- c(model$ci.lb, model$ci.ub)
  OutcomeType <- model$measure

}






#==============================================================================

  # NoPeople = number of people to be displayed in the chart
  # DesireEvent = is the outcome/event favourable/desirable (<TRUE/FALSE>)
  # OutcomeName
  # TreatmentName
  # OutcomeType (Relative Risk, Odds Ratio, Risk Difference)
  # RelEff = Relative effect point estimate calculated from meta-analysis
  # RelConfInt = Relative effect confidence interval <c(a,b)>
  # ComProb = Probability of event in comparator group
  # ComConfInt = Confidence interval of probability of event in comparator group

PopViz <- function(NoPeople, DesireEvent, OutcomeName, TreatmentName, ComparatorName,
                   OutcomeType, RelEff, RelConfInt, ComProb) {

  if (OutcomeType = "RD") {

    TrtProb <- ComProb + RelEff
    TrtConfInt <- ComConfInt + RelConfInt

  }

  if (OutcomeType = "RR") {

    TrtProb <- ComProb * RelEff
    TrtConfInt <- ComConfInt * RelConfInt

  }

  if (OutcomeType = "OR") {

    TrtProb <- (ComProb * RelEff) / (1 - ComProb + ComProb * RelEff)
    TrtConfInt <- (ComConfInt * RelConfInt) / (1 - ComConfInt + ComConfInt * RelConfInt)

  }

}
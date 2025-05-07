library(ggplot2)

#=============================================================================

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

  if (OutcomeType == "RD") {

    TrtProb <- ComProb + RelEff
    TrtConfInt <- ComConfInt + RelConfInt

  }

  if (OutcomeType == "RR") {

    TrtProb <- ComProb * RelEff
    TrtConfInt <- ComConfInt * RelConfInt

  }

  if (OutcomeType == "OR") {

    TrtProb <- (ComProb * RelEff) / (1 - ComProb + ComProb * RelEff)
    TrtConfInt <- (ComConfInt * RelConfInt) / (1 - ComConfInt + ComConfInt * RelConfInt)

  }

  PeoplePos <- data.frame(
    x = 1:100,
    y = rep(1,100)
  )

  LinePos1 <- data.frame(
   x = c(0,100),
   y = c(0.25,0.25)
  )

  LinePos2 <- data.frame(
    x = c(0,100),
    y = c(1.75,1.75)
  )

  svg_text <- .GetSvgText()
  ggplot() +
    ggsvg::geom_point_svg(
      data = PeoplePos,
      mapping  = aes(x, y),
      svg      = svg_text,
      size     = 3
    ) +

    geom_line(
      data = LinePos1,
      mapping = aes(x,y),
      linewidth = 2
    ) +

    geom_line(
      data = LinePos2,
      mapping = aes(x,y)
    ) +

    ylim(-1,3) + xlim(0,120) +

    annotate("text", x = 110, y = 1.75, label = ComparatorName) +

    theme_void()

}